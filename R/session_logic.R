# Game-loop operations on top of the SQLite store (R/db.R) and the engine
# (R/engine/). Pure of Shiny — every function takes a connection and returns
# data, so the whole flow is testable headless (see tests/testthat/test-session-logic.R).
#
# Session phases:  lobby -> open -> scored -> open -> ... -> finished

# --- session lifecycle --------------------------------------------------------

sl_create_session <- function(con, game_id, games_path = "config/games.yaml", label = NULL) {
  game <- resolve_benchmarks(get_game(game_id, games_path))
  cfg  <- jsonlite::toJSON(game, auto_unbox = TRUE, null = "null", digits = NA)
  label <- trimws(label %||% "")
  db_run(con,
    "INSERT INTO sessions (game_id, label, phase, current_round, n_rounds, config_json, created_at)
     VALUES (?, ?, 'lobby', 0, ?, ?, ?)",
    list(game_id, if (nzchar(label)) label else NA_character_,
         as.integer(game$rounds), as.character(cfg), now_epoch()))
  sid <- db_last_id(con)
  meta_set(con, "active_session_id", sid)
  sid
}

sl_session <- function(con, session_id) {
  db_one(con, "SELECT * FROM sessions WHERE session_id = ?", list(session_id))
}

sl_active_session <- function(con) {
  sid <- meta_get(con, "active_session_id")
  if (is.null(sid)) return(NULL)
  sl_session(con, as.integer(sid))
}

sl_game <- function(session_row) {
  jsonlite::fromJSON(session_row$config_json, simplifyVector = TRUE)
}

# --- history (all past sessions) -----------------------------------------------

sl_list_sessions <- function(con) {
  active <- meta_get(con, "active_session_id")
  active <- if (is.null(active) || !nzchar(active)) -1L else as.integer(active)
  d <- db_query(con, "
    SELECT s.session_id, s.label, s.game_id, s.phase, s.current_round, s.n_rounds,
           s.created_at,
           (SELECT COUNT(*) FROM players p WHERE p.session_id = s.session_id) AS n_players,
           (SELECT COUNT(*) FROM results r WHERE r.session_id = s.session_id) AS n_result_rows
    FROM sessions s ORDER BY s.session_id DESC")
  if (nrow(d)) d$is_active <- d$session_id == active
  d
}

sl_set_label <- function(con, session_id, label) {
  label <- trimws(label %||% "")
  db_run(con, "UPDATE sessions SET label = ? WHERE session_id = ?",
         list(if (nzchar(label)) label else NA_character_, session_id))
  invisible(TRUE)
}

sl_set_active <- function(con, session_id) {
  if (is.null(sl_session(con, session_id))) stop("no such session", call. = FALSE)
  meta_set(con, "active_session_id", session_id)
  invisible(TRUE)
}

sl_clear_active <- function(con) meta_set(con, "active_session_id", "")

sl_delete_session <- function(con, session_id) {
  db_run(con, "DELETE FROM submissions WHERE session_id = ?", list(session_id))
  db_run(con, "DELETE FROM results     WHERE session_id = ?", list(session_id))
  db_run(con, "DELETE FROM players     WHERE session_id = ?", list(session_id))
  db_run(con, "DELETE FROM sessions    WHERE session_id = ?", list(session_id))
  if (identical(meta_get(con, "active_session_id"), as.character(session_id)))
    sl_clear_active(con)
  invisible(TRUE)
}

sl_reset_to_lobby <- function(con, session_id) {
  db_run(con, "DELETE FROM submissions WHERE session_id = ?", list(session_id))
  db_run(con, "DELETE FROM results     WHERE session_id = ?", list(session_id))
  db_run(con, "UPDATE players SET market_id = NULL, firm_label = NULL WHERE session_id = ?",
         list(session_id))
  db_run(con, "UPDATE sessions SET phase = 'lobby', current_round = 0 WHERE session_id = ?",
         list(session_id))
  invisible(TRUE)
}

# --- players ----------------------------------------------------------------

sl_add_player <- function(con, session_id, display_name) {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "lobby") stop("the game has already started", call. = FALSE)
  name <- trimws(display_name)
  if (!nzchar(name)) stop("please enter a name", call. = FALSE)
  dup <- db_one(con,
    "SELECT 1 FROM players WHERE session_id = ? AND lower(display_name) = lower(?)",
    list(session_id, name))
  if (!is.null(dup)) stop("that name is already taken", call. = FALSE)

  token <- random_token()
  db_run(con,
    "INSERT INTO players (session_id, display_name, token, joined_at) VALUES (?, ?, ?, ?)",
    list(session_id, name, token, now_epoch()))
  list(player_id = db_last_id(con), token = token)
}

sl_player_by_token <- function(con, token) {
  if (is.null(token) || !nzchar(token)) return(NULL)
  db_one(con, "SELECT * FROM players WHERE token = ?", list(token))
}

sl_players <- function(con, session_id) {
  db_query(con, "SELECT * FROM players WHERE session_id = ? ORDER BY joined_at", list(session_id))
}

# --- start ----------------------------------------------------------------

sl_start_game <- function(con, session_id, seed = NULL) {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "lobby") stop("game already started", call. = FALSE)
  pl <- sl_players(con, session_id)
  if (nrow(pl) < 2) stop("need at least 2 players", call. = FALSE)

  pr <- make_pairs(pl$player_id, seed = seed)
  for (i in seq_len(nrow(pr$pairs))) {
    db_run(con,
      "UPDATE players SET market_id = ?, firm_label = ? WHERE player_id = ?",
      list(pr$pairs$market_id[i], pr$pairs$firm_label[i], pr$pairs$player_id[i]))
  }
  db_run(con, "UPDATE sessions SET phase = 'open', current_round = 1 WHERE session_id = ?",
         list(session_id))
  invisible(list(n_markets = nrow(pr$pairs) / 2, sitting_out = pr$sitting_out))
}

# --- submissions --------------------------------------------------------------

sl_submit <- function(con, session_id, player_id, round, price, rationale = "") {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "open") stop("the round is not open for submissions", call. = FALSE)
  if (round != s$current_round) stop("that is not the current round", call. = FALSE)
  p <- db_one(con, "SELECT market_id FROM players WHERE player_id = ?", list(player_id))
  if (is.null(p)) stop("unknown player", call. = FALSE)
  if (is.null(p$market_id) || is.na(p$market_id))
    stop("you are not in a market this game", call. = FALSE)
  price <- suppressWarnings(as.numeric(price))
  if (is.na(price) || price < 0) stop("enter a price of 0 or more", call. = FALSE)

  db_run(con,
    "INSERT INTO submissions (session_id, round, player_id, price, rationale, carried, submitted_at)
     VALUES (?, ?, ?, ?, ?, 0, ?)
     ON CONFLICT(session_id, round, player_id)
     DO UPDATE SET price = excluded.price, rationale = excluded.rationale,
                   carried = 0, submitted_at = excluded.submitted_at",
    list(session_id, round, player_id, price, as.character(rationale %||% ""), now_epoch()))
  invisible(TRUE)
}

sl_round_status <- function(con, session_id) {
  s <- sl_session(con, session_id)
  rnd <- s$current_round
  expected <- db_query(con,
    "SELECT player_id, display_name FROM players
     WHERE session_id = ? AND market_id IS NOT NULL", list(session_id))
  submitted <- db_query(con,
    "SELECT player_id FROM submissions WHERE session_id = ? AND round = ?",
    list(session_id, rnd))$player_id
  list(
    round        = rnd,
    n_expected   = nrow(expected),
    n_submitted  = sum(expected$player_id %in% submitted),
    missing      = expected$display_name[!expected$player_id %in% submitted]
  )
}

# --- scoring ----------------------------------------------------------------

.prev_price <- function(con, session_id, player_id, round) {
  v <- db_one(con,
    "SELECT price FROM submissions WHERE session_id = ? AND player_id = ? AND round = ?",
    list(session_id, player_id, round - 1))
  if (!is.null(v)) return(v$price)
  v <- db_one(con,
    "SELECT price FROM results WHERE session_id = ? AND player_id = ? AND round = ?",
    list(session_id, player_id, round - 1))
  if (!is.null(v)) v$price else NULL
}

# A missing price is treated as "priced yourself out of the market": the firm gets
# no sales and $0, while its rival gets the residual-monopoly outcome at its own
# price. This means a diligent player is never punished for a rival's no-show.
# `carry_forward = TRUE` instead reuses that player's previous-round price.
sl_score_round <- function(con, session_id, carry_forward = FALSE) {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  # allowed while the round is open or already scored, so the admin can re-score
  if (!s$phase %in% c("open", "scored")) stop("round is not open", call. = FALSE)
  game   <- sl_game(s)
  model  <- game$demand$model
  params <- game$demand$params
  rnd    <- s$current_round
  out_price <- (params$V %||% params$choke %||% 100) * 1e6   # finite "out of market"

  pl <- db_query(con,
    "SELECT player_id, display_name, market_id, firm_label FROM players
     WHERE session_id = ? AND market_id IS NOT NULL ORDER BY market_id, firm_label",
    list(session_id))

  scored <- 0L; skipped <- character(0); no_shows <- character(0)

  for (mk in sort(unique(pl$market_id))) {
    firms <- pl[pl$market_id == mk, , drop = FALSE]
    if (nrow(firms) != 2) { skipped <- c(skipped, sprintf("market %s", mk)); next }

    prices  <- rep(NA_real_, 2)
    missing <- rep(FALSE, 2)
    for (j in 1:2) {
      sub <- db_one(con,
        "SELECT price FROM submissions WHERE session_id = ? AND round = ? AND player_id = ?",
        list(session_id, rnd, firms$player_id[j]))
      if (!is.null(sub)) {
        prices[j] <- sub$price
        next
      }
      pp <- if (isTRUE(carry_forward)) .prev_price(con, session_id, firms$player_id[j], rnd) else NULL
      if (!is.null(pp)) {
        prices[j] <- pp
        db_run(con,
          "INSERT INTO submissions (session_id, round, player_id, price, rationale, carried, submitted_at)
           VALUES (?, ?, ?, ?, '(carried forward)', 1, ?)
           ON CONFLICT(session_id, round, player_id) DO NOTHING",
          list(session_id, rnd, firms$player_id[j], pp, now_epoch()))
      } else {
        # No submission and nothing to carry: score them out of the market. No
        # submissions row is written, so a later carry_forward re-score can still
        # fill the gap; the results row carries the no_submission flag.
        prices[j]  <- out_price
        missing[j] <- TRUE
        no_shows   <- c(no_shows, firms$display_name[j])
      }
    }

    res <- resolve_round(model, params,
             data.frame(firm = firms$firm_label, price = prices, stringsAsFactors = FALSE))

    for (j in 1:2) {
      prior <- db_one(con,
        "SELECT COALESCE(SUM(profit), 0) AS s FROM results
         WHERE session_id = ? AND player_id = ? AND round < ?",
        list(session_id, firms$player_id[j], rnd))$s
      own_price   <- if (missing[j]) NA_real_ else res$price[j]
      rival_price <- if (missing[3 - j]) NA_real_ else res$rival_price[j]
      db_run(con,
        "INSERT INTO results
           (session_id, round, player_id, market_id, price, rival_price, share,
            quantity, revenue, profit, cumulative_profit, no_submission, computed_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(session_id, round, player_id) DO UPDATE SET
           price=excluded.price, rival_price=excluded.rival_price, share=excluded.share,
           quantity=excluded.quantity, revenue=excluded.revenue, profit=excluded.profit,
           cumulative_profit=excluded.cumulative_profit,
           no_submission=excluded.no_submission, computed_at=excluded.computed_at",
        list(session_id, rnd, firms$player_id[j], mk,
             own_price, rival_price, res$share[j], res$quantity[j],
             res$revenue[j], res$profit[j], prior + res$profit[j],
             as.integer(missing[j]), now_epoch()))
    }
    scored <- scored + 1L
  }

  db_run(con, "UPDATE sessions SET phase = 'scored' WHERE session_id = ?", list(session_id))
  invisible(list(markets_scored = scored, skipped = skipped, no_shows = unique(no_shows)))
}

sl_advance_round <- function(con, session_id) {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "scored") stop("score the round first", call. = FALSE)
  if (s$current_round >= s$n_rounds) {
    db_run(con, "UPDATE sessions SET phase = 'finished' WHERE session_id = ?", list(session_id))
    return(invisible("finished"))
  }
  db_run(con,
    "UPDATE sessions SET phase = 'open', current_round = current_round + 1 WHERE session_id = ?",
    list(session_id))
  invisible("open")
}

# --- read models for the UI -------------------------------------------------

sl_state_stamp <- function(con, session_id) {
  s <- sl_session(con, session_id)
  if (is.null(s)) return("none")
  n_pl  <- db_query(con, "SELECT COUNT(*) n FROM players WHERE session_id = ?", list(session_id))$n
  # MAX(submitted_at) so an *edit* to an existing submission (same row count) also
  # bumps the stamp; MAX(computed_at) so a re-score does too.
  sub <- db_query(con,
    "SELECT COUNT(*) n, COALESCE(MAX(submitted_at), 0) t
       FROM submissions WHERE session_id = ? AND round = ?",
    list(session_id, s$current_round))
  res <- db_query(con,
    "SELECT COUNT(*) n, COALESCE(MAX(computed_at), 0) t
       FROM results WHERE session_id = ?", list(session_id))
  paste(s$phase, s$current_round, n_pl,
        sub$n, sprintf("%.3f", sub$t), res$n, sprintf("%.3f", res$t), sep = "|")
}

sl_load_state <- function(con, session_id) {
  s <- sl_session(con, session_id)
  if (is.null(s)) return(NULL)
  list(
    session      = s,
    game         = sl_game(s),
    players      = sl_players(con, session_id),
    round_status = sl_round_status(con, session_id),
    submissions  = db_query(con,
      "SELECT * FROM submissions WHERE session_id = ? AND round = ?",
      list(session_id, s$current_round)),
    results      = db_query(con,
      "SELECT r.*, p.display_name FROM results r
       JOIN players p ON p.player_id = r.player_id
       WHERE r.session_id = ? ORDER BY r.round, r.market_id, r.player_id",
      list(session_id))
  )
}

# Full long results + rationale, for CSV export.
sl_export_frame <- function(con, session_id) {
  db_query(con,
    "SELECT r.round, r.market_id, p.display_name, pl_firm.firm_label,
            r.price, r.rival_price, r.share, r.quantity, r.revenue,
            r.profit, r.cumulative_profit, r.no_submission,
            s.rationale, s.carried
     FROM results r
     JOIN players p       ON p.player_id = r.player_id
     JOIN players pl_firm ON pl_firm.player_id = r.player_id
     LEFT JOIN submissions s
            ON s.session_id = r.session_id AND s.round = r.round AND s.player_id = r.player_id
     WHERE r.session_id = ?
     ORDER BY r.round, r.market_id, pl_firm.firm_label",
    list(session_id))
}
