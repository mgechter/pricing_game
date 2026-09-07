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

# Run a started session to completion. `price` is a function(round, firm_label,
# player_id) -> numeric, or a single number.
play_out <- function(con, sid, price = 20, carry_forward = FALSE) {
  pf <- if (is.function(price)) price else function(...) price
  repeat {
    s <- sl_session(con, sid)
    if (s$phase == "finished") break
    if (s$phase == "open") {
      pl <- sl_players(con, sid)
      pl <- pl[!is.na(pl$market_id), ]
      for (i in seq_len(nrow(pl)))
        sl_submit(con, sid, pl$player_id[i], s$current_round,
                  pf(s$current_round, pl$firm_label[i], pl$player_id[i]))
      sl_score_round(con, sid, carry_forward = carry_forward)
    }
    sl_advance_round(con, sid)
  }
  invisible(sid)
}
