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
  db_run(con,
    "UPDATE players SET market_id = NULL, firm_label = NULL, shadow_of = NULL
     WHERE session_id = ?", list(session_id))
  db_run(con,
    "UPDATE sessions SET phase = 'lobby', current_round = 0, round_started_at = NULL
     WHERE session_id = ?", list(session_id))
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

  token <- NULL
  for (i in 1:10) {
    cand <- random_token()
    if (is.null(db_one(con, "SELECT 1 FROM players WHERE token = ?", list(cand)))) {
      token <- cand; break
    }
  }
  if (is.null(token)) stop("could not allocate a unique token", call. = FALSE)
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
    row <- pr$pairs[i, ]
    db_run(con,
      "UPDATE players SET market_id = ?, firm_label = ?, shadow_of = ? WHERE player_id = ?",
      list(row$market_id, row$firm_label,
           if (is.na(row$shadow_of)) NA else as.integer(row$shadow_of), row$player_id))
  }
  db_run(con,
    "UPDATE sessions SET phase = 'open', current_round = 1, round_started_at = ?
     WHERE session_id = ?",
    list(now_epoch(), session_id))
  invisible(list(n_markets = sum(is.na(pr$pairs$shadow_of)) / 2, shadow = pr$shadow))
}

# --- timer / display settings ------------------------------------------------

sl_set_timer <- function(con, session_id, seconds, auto_advance = NULL) {
  seconds <- max(0L, as.integer(seconds %||% 0L))
  db_run(con, "UPDATE sessions SET round_seconds = ? WHERE session_id = ?",
         list(seconds, session_id))
  if (!is.null(auto_advance))
    db_run(con, "UPDATE sessions SET auto_advance = ? WHERE session_id = ?",
           list(as.integer(isTRUE(auto_advance)), session_id))
  invisible(TRUE)
}

sl_set_anonymous <- function(con, session_id, anonymous) {
  db_run(con, "UPDATE sessions SET anonymous = ? WHERE session_id = ?",
         list(as.integer(isTRUE(anonymous)), session_id))
  invisible(TRUE)
}

sl_set_projector <- function(con, session_id, view) {
  view <- match.arg(as.character(view), c("summary", "scoreboard", "detail"))
  db_run(con, "UPDATE sessions SET projector_view = ? WHERE session_id = ?",
         list(view, session_id))
  invisible(TRUE)
}

# seconds left in the current round, or NA if no timer / not open
sl_time_left <- function(s) {
  if (is.null(s) || s$phase != "open" || (s$round_seconds %||% 0) <= 0) return(NA_real_)
  s$round_seconds - (now_epoch() - (s$round_started_at %||% now_epoch()))
}

# --- submissions --------------------------------------------------------------

# `confirm = TRUE` marks the round done for this student (sticky — a later
# auto-save with confirm = FALSE does not clear it).
sl_submit <- function(con, session_id, player_id, round, price, rationale = "",
                      confirm = FALSE) {
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
    "INSERT INTO submissions (session_id, round, player_id, price, rationale, carried, confirmed, submitted_at)
     VALUES (?, ?, ?, ?, ?, 0, ?, ?)
     ON CONFLICT(session_id, round, player_id)
     DO UPDATE SET price = excluded.price, rationale = excluded.rationale, carried = 0,
                   confirmed = MAX(submissions.confirmed, excluded.confirmed),
                   submitted_at = excluded.submitted_at",
    list(session_id, round, player_id, price, as.character(rationale %||% ""),
         as.integer(isTRUE(confirm)), now_epoch()))
  invisible(TRUE)
}

sl_round_status <- function(con, session_id) {
  s <- sl_session(con, session_id)
  rnd <- s$current_round
  expected <- db_query(con,
    "SELECT player_id, display_name FROM players
     WHERE session_id = ? AND market_id IS NOT NULL", list(session_id))
  subs <- db_query(con,
    "SELECT player_id, confirmed FROM submissions WHERE session_id = ? AND round = ?",
    list(session_id, rnd))
  priced    <- expected$player_id %in% subs$player_id
  confirmed <- expected$player_id %in% subs$player_id[subs$confirmed == 1]
  list(
    round         = rnd,
    n_expected    = nrow(expected),
    n_priced      = sum(priced),
    n_confirmed   = sum(confirmed),
    n_submitted   = sum(priced),               # back-compat alias
    unconfirmed   = expected$display_name[!confirmed],
    missing       = expected$display_name[!priced]
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

# The price a player is scored at this round: their submission, or (with
# carry_forward) their previous price, or a finite "out of the market" price when
# there is nothing. Writes the carried-forward submission row as a side effect.
.effective_price <- function(con, session_id, player_id, round, carry_forward, out_price) {
  sub <- db_one(con,
    "SELECT price FROM submissions WHERE session_id = ? AND round = ? AND player_id = ?",
    list(session_id, round, player_id))
  if (!is.null(sub)) return(list(price = sub$price, missing = FALSE))
  if (isTRUE(carry_forward)) {
    pp <- .prev_price(con, session_id, player_id, round)
    if (!is.null(pp)) {
      db_run(con,
        "INSERT INTO submissions (session_id, round, player_id, price, rationale, carried, submitted_at)
         VALUES (?, ?, ?, ?, '(carried forward)', 1, ?)
         ON CONFLICT(session_id, round, player_id) DO NOTHING",
        list(session_id, round, player_id, pp, now_epoch()))
      return(list(price = pp, missing = FALSE))
    }
  }
  list(price = out_price, missing = TRUE)
}

.write_result <- function(con, session_id, round, player_id, market_id,
                          own_price, rival_price, r, missing) {
  prior <- db_one(con,
    "SELECT COALESCE(SUM(profit), 0) AS s FROM results
     WHERE session_id = ? AND player_id = ? AND round < ?",
    list(session_id, player_id, round))$s
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
    list(session_id, round, player_id, market_id, own_price, rival_price,
         r$share, r$quantity, r$revenue, r$profit, prior + r$profit,
         as.integer(missing), now_epoch()))
}

# Score the current round. A missing price is treated as "priced yourself out of
# the market" ($0; the rival gets the sole-seller outcome) unless carry_forward
# reuses the player's previous price. Shadow players are scored 1-on-1 against
# their host's effective price for the round; the host is not scored twice.
sl_score_round <- function(con, session_id, carry_forward = FALSE) {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (!s$phase %in% c("open", "scored")) stop("round is not open", call. = FALSE)
  game   <- sl_game(s)
  model  <- game$demand$model
  params <- game$demand$params
  rnd    <- s$current_round
  out_price <- (params$V %||% params$choke %||% 100) * 1e6

  pl <- db_query(con,
    "SELECT player_id, display_name, market_id, firm_label, shadow_of FROM players
     WHERE session_id = ? AND market_id IS NOT NULL ORDER BY market_id, firm_label",
    list(session_id))
  real    <- pl[is.na(pl$shadow_of), , drop = FALSE]
  shadows <- pl[!is.na(pl$shadow_of), , drop = FALSE]

  scored <- 0L; skipped <- character(0); no_shows <- character(0)

  for (mk in sort(unique(real$market_id))) {
    firms <- real[real$market_id == mk, , drop = FALSE]
    if (nrow(firms) != 2) { skipped <- c(skipped, sprintf("market %s", mk)); next }
    ep <- lapply(1:2, function(j)
      .effective_price(con, session_id, firms$player_id[j], rnd, carry_forward, out_price))
    prices  <- vapply(ep, `[[`, numeric(1), "price")
    missing <- vapply(ep, `[[`, logical(1), "missing")
    for (j in 1:2) if (missing[j]) no_shows <- c(no_shows, firms$display_name[j])

    res <- resolve_round(model, params,
             data.frame(firm = firms$firm_label, price = prices, stringsAsFactors = FALSE))
    for (j in 1:2)
      .write_result(con, session_id, rnd, firms$player_id[j], mk,
        if (missing[j]) NA_real_ else res$price[j],
        if (missing[3 - j]) NA_real_ else res$rival_price[j],
        res[j, ], missing[j])
    scored <- scored + 1L
  }

  for (i in seq_len(nrow(shadows))) {
    sh <- shadows[i, ]
    host <- db_one(con,
      "SELECT price FROM results WHERE session_id = ? AND player_id = ? AND round = ?",
      list(session_id, sh$shadow_of, rnd))
    host_out   <- is.null(host) || is.na(host$price)
    host_price <- if (host_out) out_price else host$price
    ep <- .effective_price(con, session_id, sh$player_id, rnd, carry_forward, out_price)
    if (ep$missing) no_shows <- c(no_shows, sh$display_name)

    res <- resolve_round(model, params,
             data.frame(firm = c("Shadow", "Host"), price = c(ep$price, host_price),
                        stringsAsFactors = FALSE))
    .write_result(con, session_id, rnd, sh$player_id, sh$market_id,
      if (ep$missing) NA_real_ else ep$price,
      if (host_out) NA_real_ else host_price,
      res[1, ], ep$missing)
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
    "UPDATE sessions SET phase = 'open', current_round = current_round + 1,
            round_started_at = ? WHERE session_id = ?",
    list(now_epoch(), session_id))
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
        sub$n, sprintf("%.3f", sub$t), res$n, sprintf("%.3f", res$t),
        s$round_seconds %||% 0, s$auto_advance %||% 0, s$anonymous %||% 0,
        s$projector_view %||% "summary",
        sep = "|")
}

# Timer has run out on an open round (checked by the admin app while connected).
# When true the admin auto-closes and scores the round; if `auto_advance` is also
# set it then moves to the next round, otherwise it waits for the instructor.
sl_should_auto_score <- function(s) {
  (s$round_seconds %||% 0) > 0 && s$phase == "open" &&
    !is.na(tl <- sl_time_left(s)) && tl <= 0
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
            s.rationale, s.carried, s.confirmed
     FROM results r
     JOIN players p       ON p.player_id = r.player_id
     JOIN players pl_firm ON pl_firm.player_id = r.player_id
     LEFT JOIN submissions s
            ON s.session_id = r.session_id AND s.round = r.round AND s.player_id = r.player_id
     WHERE r.session_id = ?
     ORDER BY r.round, r.market_id, pl_firm.firm_label",
    list(session_id))
}
