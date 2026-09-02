# Shared helpers for the DB / session / UI tests.

fresh_db <- function(env = parent.frame()) {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  db_init(con)
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

join <- function(con, sid, names) {
  vapply(names, function(nm) sl_add_player(con, sid, nm)$player_id, integer(1),
         USE.NAMES = FALSE)
}
