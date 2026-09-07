# SQLite persistence for the pricing game.
#
# One file, one process (Shiny serialises all sessions on a single R thread), so
# there is no real write concurrency — WAL + a busy timeout cover the edge cases.
# Higher-level game logic lives in R/session_logic.R; this file is just the store.

DB_SCHEMA <- "
CREATE TABLE IF NOT EXISTS meta (
  key   TEXT PRIMARY KEY,
  value TEXT
);
CREATE TABLE IF NOT EXISTS sessions (
  session_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  game_id          TEXT    NOT NULL,
  label            TEXT,
  phase            TEXT    NOT NULL DEFAULT 'lobby',   -- lobby | open | scored | finished
  current_round    INTEGER NOT NULL DEFAULT 0,
  n_rounds         INTEGER NOT NULL,
  config_json      TEXT    NOT NULL,
  round_started_at REAL,                              -- epoch when the current round opened
  round_seconds    INTEGER NOT NULL DEFAULT 0,        -- countdown length; 0 = no timer
  auto_advance     INTEGER NOT NULL DEFAULT 0,        -- also advance after the timer auto-scores
  anonymous        INTEGER NOT NULL DEFAULT 0,        -- hide names on projector / debrief
  projector_view   TEXT    NOT NULL DEFAULT 'summary', -- 'summary' | 'scoreboard' | 'detail'
  created_at       REAL    NOT NULL
);
CREATE TABLE IF NOT EXISTS players (
  player_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id   INTEGER NOT NULL,
  display_name TEXT    NOT NULL,
  token        TEXT    NOT NULL UNIQUE,
  market_id    INTEGER,                             -- NULL = unpaired
  firm_label   TEXT,                                -- 'Firm A' | 'Firm B' | 'Shadow'
  shadow_of    INTEGER,                             -- host player_id if this is a shadow
  joined_at    REAL    NOT NULL
);
CREATE TABLE IF NOT EXISTS submissions (
  session_id   INTEGER NOT NULL,
  round        INTEGER NOT NULL,
  player_id    INTEGER NOT NULL,
  price        REAL    NOT NULL,
  rationale    TEXT,
  carried      INTEGER NOT NULL DEFAULT 0,          -- 1 = auto carried-forward at scoring
  confirmed    INTEGER NOT NULL DEFAULT 0,          -- 1 = student pressed Submit (done)
  submitted_at REAL    NOT NULL,
  PRIMARY KEY (session_id, round, player_id)
);
CREATE TABLE IF NOT EXISTS results (
  session_id        INTEGER NOT NULL,
  round             INTEGER NOT NULL,
  player_id         INTEGER NOT NULL,
  market_id         INTEGER NOT NULL,
  price             REAL,
  rival_price       REAL,
  share             REAL,
  quantity          REAL,
  revenue           REAL,
  profit            REAL,
  cumulative_profit REAL,
  no_submission     INTEGER NOT NULL DEFAULT 0,
  computed_at       REAL    NOT NULL,
  PRIMARY KEY (session_id, round, player_id)
);
"

# Columns added after the first release — applied to pre-existing DBs.
DB_MIGRATIONS <- c(
  "ALTER TABLE results  ADD COLUMN no_submission INTEGER NOT NULL DEFAULT 0",
  "ALTER TABLE sessions ADD COLUMN label TEXT",
  "ALTER TABLE players  ADD COLUMN shadow_of INTEGER",
  "ALTER TABLE sessions ADD COLUMN round_started_at REAL",
  "ALTER TABLE sessions ADD COLUMN round_seconds INTEGER NOT NULL DEFAULT 0",
  "ALTER TABLE sessions ADD COLUMN auto_advance INTEGER NOT NULL DEFAULT 0",
  "ALTER TABLE sessions ADD COLUMN anonymous INTEGER NOT NULL DEFAULT 0",
  "ALTER TABLE sessions ADD COLUMN projector_view TEXT NOT NULL DEFAULT 'summary'",
  "ALTER TABLE submissions ADD COLUMN confirmed INTEGER NOT NULL DEFAULT 0"
)

db_connect <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

db_init <- function(con) {
  for (stmt in strsplit(DB_SCHEMA, ";\\s*\n")[[1]]) {
    stmt <- trimws(stmt)
    if (nzchar(stmt)) DBI::dbExecute(con, stmt)
  }
  for (stmt in DB_MIGRATIONS) {
    tryCatch(DBI::dbExecute(con, stmt), error = function(e) NULL)  # already applied
  }
  invisible(TRUE)
}

# --- thin query helpers ---------------------------------------------------

db_run <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbExecute(con, sql, params = params)
  else                DBI::dbExecute(con, sql)
}

db_query <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbGetQuery(con, sql, params = params)
  else                DBI::dbGetQuery(con, sql)
}

db_one <- function(con, sql, params = list()) {
  d <- db_query(con, sql, params)
  if (nrow(d) == 0) NULL else as.list(d[1, , drop = FALSE])
}

db_last_id <- function(con) {
  db_query(con, "SELECT last_insert_rowid() AS id")$id
}

meta_get <- function(con, key) {
  v <- db_one(con, "SELECT value FROM meta WHERE key = ?", list(key))
  if (is.null(v)) NULL else v$value
}

meta_set <- function(con, key, value) {
  db_run(con,
    "INSERT INTO meta (key, value) VALUES (?, ?)
     ON CONFLICT(key) DO UPDATE SET value = excluded.value",
    list(key, as.character(value)))
}

now_epoch <- function() as.numeric(Sys.time())

random_token <- function(n = 12) {
  paste(sample(c(letters, 0:9), n, replace = TRUE), collapse = "")
}
