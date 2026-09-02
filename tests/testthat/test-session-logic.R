# End-to-end game loop against a throwaway SQLite file — no Shiny.
# fresh_db() / join() live in helper-game.R.

test_that("a full 4-player Bertrand game runs lobby -> finished", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  expect_equal(sl_session(con, sid)$phase, "lobby")

  ids <- join(con, sid, c("Ada", "Boaz", "Cleo", "Dax"))
  sl_start_game(con, sid, seed = 42)

  s <- sl_session(con, sid)
  expect_equal(s$phase, "open")
  expect_equal(s$current_round, 1)
  pl <- sl_players(con, sid)
  expect_setequal(pl$market_id, c(1, 1, 2, 2))
  expect_setequal(pl$firm_label, c("Firm A", "Firm B", "Firm A", "Firm B"))

  # round 1: within each market, the lower price takes the whole market (Q = 100 - p)
  m1 <- pl$player_id[pl$market_id == 1]
  m2 <- pl$player_id[pl$market_id == 2]
  sl_submit(con, sid, m1[1], 1, 40); sl_submit(con, sid, m1[2], 1, 60)
  sl_submit(con, sid, m2[1], 1, 30); sl_submit(con, sid, m2[2], 1, 30)

  st <- sl_round_status(con, sid)
  expect_equal(st$n_submitted, 4)
  expect_equal(st$n_expected, 4)

  sc <- sl_score_round(con, sid)
  expect_equal(sc$markets_scored, 2)
  res1 <- sl_load_state(con, sid)$results
  low  <- res1[res1$player_id == m1[1], ]
  expect_equal(low$quantity, 60)        # 100 - 40
  expect_equal(low$profit, 60 * 40)
  expect_equal(res1$profit[res1$player_id == m1[2]], 0)
  # tie in market 2 splits
  expect_equal(sort(res1$profit[res1$market_id == 2]), c(30 * 35, 30 * 35))

  expect_equal(sl_session(con, sid)$phase, "scored")
  sl_advance_round(con, sid)
  expect_equal(sl_session(con, sid)$current_round, 2)

  # play rounds 2..10 quickly
  for (r in 2:10) {
    for (pid in ids) sl_submit(con, sid, pid, r, 20)
    sl_score_round(con, sid)
    out <- sl_advance_round(con, sid)
  }
  expect_equal(out, "finished")
  expect_equal(sl_session(con, sid)$phase, "finished")

  fin <- sl_load_state(con, sid)$results
  # cumulative profit is monotone per player and equals the running sum
  for (pid in ids) {
    pr <- fin[fin$player_id == pid, ]
    pr <- pr[order(pr$round), ]
    expect_equal(pr$cumulative_profit, cumsum(pr$profit))
  }
})

test_that("odd headcount leaves one player sitting out", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, c("a", "b", "c", "d", "e"))
  info <- sl_start_game(con, sid, seed = 1)
  expect_length(info$sitting_out, 1)
  pl <- sl_players(con, sid)
  expect_equal(sum(is.na(pl$market_id)), 1)
  expect_equal(sum(!is.na(pl$market_id)), 4)

  out_id <- pl$player_id[is.na(pl$market_id)]
  expect_error(sl_submit(con, sid, out_id, 1, 10), "not in a market")
})

test_that("a no-show is scored out of the market; the diligent rival is not punished", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("a", "b"))
  sl_start_game(con, sid, seed = 7)

  sl_submit(con, sid, ids[1], 1, 30)          # b does not submit
  sc <- sl_score_round(con, sid)              # default: no carry-forward
  expect_equal(sc$markets_scored, 1)
  expect_length(sc$skipped, 0)
  expect_equal(sc$no_shows, "b")

  res <- sl_load_state(con, sid)$results
  a <- res[res$player_id == ids[1], ]; b <- res[res$player_id == ids[2], ]
  expect_equal(a$profit, 30 * (100 - 30))     # sole seller at its own price
  expect_true(is.na(a$rival_price))
  expect_equal(b$profit, 0)
  expect_equal(b$no_submission, 1L)
  expect_true(is.na(b$price))
})

test_that("carry_forward re-score fills a missing price from the prior round", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("a", "b"))
  sl_start_game(con, sid, seed = 7)

  sl_submit(con, sid, ids[1], 1, 25); sl_submit(con, sid, ids[2], 1, 40)
  sl_score_round(con, sid); sl_advance_round(con, sid)

  sl_submit(con, sid, ids[1], 2, 20)          # only a submits in round 2
  sl_score_round(con, sid, carry_forward = FALSE)          # b scored out of market
  expect_equal(sl_load_state(con, sid)$results$no_submission[
    sl_load_state(con, sid)$results$player_id == ids[2] &
    sl_load_state(con, sid)$results$round == 2], 1L)

  sl_score_round(con, sid, carry_forward = TRUE)           # re-score, filling the gap
  sub2 <- db_query(con, "SELECT * FROM submissions WHERE session_id=? AND round=2", list(sid))
  expect_equal(sub2$price[sub2$player_id == ids[2]], 40)   # carried from round 1
  expect_equal(sub2$carried[sub2$player_id == ids[2]], 1)
  r2 <- sl_load_state(con, sid)$results
  expect_equal(r2$no_submission[r2$player_id == ids[2] & r2$round == 2], 0L)
})

test_that("history: list, label, delete, and switch the active session", {
  con <- fresh_db()
  s1 <- sl_create_session(con, "bertrand", GAMES_PATH, label = "keep me")
  s2 <- sl_create_session(con, "hotelling_high", GAMES_PATH)      # false start
  expect_equal(sl_active_session(con)$session_id, s2)

  h <- sl_list_sessions(con)
  expect_equal(nrow(h), 2)
  expect_equal(h$label[h$session_id == s1], "keep me")
  expect_true(h$is_active[h$session_id == s2])

  sl_set_label(con, s2, "  ")                     # blank clears the label
  expect_true(is.na(sl_list_sessions(con)$label[sl_list_sessions(con)$session_id == s2]))

  sl_delete_session(con, s2)                      # drop the false start
  h <- sl_list_sessions(con)
  expect_equal(nrow(h), 1)
  expect_equal(h$session_id, s1)
  expect_null(sl_active_session(con))             # deleting the active one clears the pointer

  sl_set_active(con, s1)
  expect_equal(sl_active_session(con)$session_id, s1)
  expect_error(sl_set_active(con, 999), "no such session")
})

test_that("guard rails: phase and lobby restrictions", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  expect_error(sl_start_game(con, sid), "at least 2 players")

  ids <- join(con, sid, c("a", "b"))
  expect_error(sl_submit(con, sid, ids[1], 1, 10), "not open for submissions")
  sl_start_game(con, sid, seed = 1)
  expect_error(sl_add_player(con, sid, "late"), "already started")
  expect_error(sl_submit(con, sid, ids[1], 2, 10), "not the current round")
  expect_error(sl_advance_round(con, sid), "score the round first")
})

test_that("state stamp changes as the game moves", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  s0 <- sl_state_stamp(con, sid)
  join(con, sid, c("a", "b"))
  expect_false(identical(s0, sl_state_stamp(con, sid)))
  ids <- sl_players(con, sid)$player_id
  sl_start_game(con, sid, seed = 1)
  s1 <- sl_state_stamp(con, sid)
  sl_submit(con, sid, ids[1], 1, 10)
  expect_false(identical(s1, sl_state_stamp(con, sid)))
})

test_that("config round-trips through JSON and drives scoring for Hotelling too", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  g <- sl_game(sl_session(con, sid))
  expect_equal(g$demand$model, "hotelling_linear")
  expect_true(isTRUE(g$demand$params$coverage_decay))

  ids <- join(con, sid, c("a", "b"))
  sl_start_game(con, sid, seed = 3)
  sl_submit(con, sid, ids[1], 1, 120); sl_submit(con, sid, ids[2], 1, 120)
  sl_score_round(con, sid)
  r <- sl_load_state(con, sid)$results
  expect_equal(r$share, c(0.5, 0.5))
  expect_equal(r$profit, c(0.5 * 100 * 120, 0.5 * 100 * 120))
})
