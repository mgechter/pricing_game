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
  nr <- sl_session(con, sid)$n_rounds
  out <- sl_advance_round(con, sid)

  # play the remaining rounds quickly (round count comes from games.yaml)
  while (identical(out, "open")) {
    r <- sl_session(con, sid)$current_round
    for (pid in ids) sl_submit(con, sid, pid, r, 20)
    sl_score_round(con, sid)
    out <- sl_advance_round(con, sid)
  }
  expect_equal(out, "finished")
  expect_equal(sl_session(con, sid)$phase, "finished")
  expect_equal(max(sl_load_state(con, sid)$results$round), nr)

  fin <- sl_load_state(con, sid)$results
  # cumulative profit is monotone per player and equals the running sum
  for (pid in ids) {
    pr <- fin[fin$player_id == pid, ]
    pr <- pr[order(pr$round), ]
    expect_equal(pr$cumulative_profit, cumsum(pr$profit))
  }
})

test_that("odd headcount -> shadow: everyone plays, host is not scored twice", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("a", "b", "c", "d", "e"))
  info <- sl_start_game(con, sid, seed = 1)

  expect_false(is.null(info$shadow))
  pl <- sl_players(con, sid)
  expect_equal(sum(is.na(pl$market_id)), 0)          # nobody benched
  sh <- pl[!is.na(pl$shadow_of), ]
  host <- pl[pl$player_id == sh$shadow_of, ]
  expect_equal(nrow(sh), 1)
  expect_equal(sh$firm_label, "Shadow")
  expect_equal(sh$market_id, host$market_id)         # sits in the host's market

  # play a round: shadow submits, gets a result 1-on-1 vs the host
  for (pid in ids) sl_submit(con, sid, pid, 1, 20 + pid)
  sc <- sl_score_round(con, sid)
  res <- sl_load_state(con, sid)$results
  expect_equal(nrow(res[res$round == 1, ]), 5)       # 4 real firms + 1 shadow
  shadow_row <- res[res$player_id == sh$player_id & res$round == 1, ]
  host_row   <- res[res$player_id == host$player_id & res$round == 1, ]
  expect_equal(nrow(shadow_row), 1)
  expect_equal(nrow(host_row), 1)                    # exactly one host result row
  # the shadow's rival price is the host's price
  expect_equal(shadow_row$rival_price, sl_load_state(con, sid)$submissions$price[
    sl_load_state(con, sid)$submissions$player_id == host$player_id])
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

test_that("shadow faces the host's effective price, incl. when the host no-shows", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)   # NE 0, choke 100
  ids <- join(con, sid, c("a", "b", "c"))
  info <- sl_start_game(con, sid, seed = 1)
  pl   <- sl_players(con, sid)
  sh   <- pl[!is.na(pl$shadow_of), ]
  host <- pl[pl$player_id == sh$shadow_of, ]
  rival_of_host <- pl[pl$market_id == host$market_id & is.na(pl$shadow_of) &
                        pl$player_id != host$player_id, ]

  # round 1: shadow prices 20; host does NOT submit -> host scored out of market,
  # so the shadow faces an absent rival and gets the sole-seller outcome.
  sl_submit(con, sid, sh$player_id, 1, 20)
  sl_submit(con, sid, rival_of_host$player_id, 1, 55)
  sl_score_round(con, sid)
  r <- sl_load_state(con, sid)$results
  sr <- r[r$player_id == sh$player_id, ]
  expect_equal(sr$profit, 20 * (100 - 20))        # sole seller at its own price
  expect_true(is.na(sr$rival_price))
})

test_that("round timer: set, count down, and flag auto-score at 0:00", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, c("a", "b"))
  expect_true(is.na(sl_time_left(sl_session(con, sid))))    # lobby

  sl_set_timer(con, sid, 60)                                # timer on, no auto-advance
  sl_start_game(con, sid, seed = 1)
  s <- sl_session(con, sid)
  expect_equal(s$round_seconds, 60)
  expect_equal(s$auto_advance, 0)
  tl <- sl_time_left(s)
  expect_true(tl > 55 && tl <= 60)
  expect_false(sl_should_auto_score(s))                     # time still left

  # pretend the round started 61 s ago
  db_run(con, "UPDATE sessions SET round_started_at = ? WHERE session_id = ?",
         list(as.numeric(Sys.time()) - 61, sid))
  s <- sl_session(con, sid)
  expect_lte(sl_time_left(s), 0)
  expect_true(sl_should_auto_score(s))                      # any timer -> auto-score on expiry

  sl_set_timer(con, sid, 45, auto_advance = TRUE)
  expect_equal(sl_session(con, sid)$auto_advance, 1)

  sl_set_timer(con, sid, 0)                                 # timer off
  expect_true(is.na(sl_time_left(sl_session(con, sid))))
  expect_false(sl_should_auto_score(sl_session(con, sid)))
})

test_that("predictions gate round 1 and lift when answered or forced", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("Ada", "Bo", "Cy", "Di"))
  sl_start_game(con, sid, seed = 1)
  s <- sl_session(con, sid); g <- sl_game(s)

  req <- game_required_qids(g)
  expect_gt(length(req), 0)
  expect_true(sl_player_predicts_pending(con, s, g, ids[1]))

  for (q in game_predictions(g))
    if (!q$optional)
      sl_set_prediction(con, sid, ids[1], q$id, if (q$type == "numeric") "30" else "Yes")
  expect_false(sl_player_predicts_pending(con, s, g, ids[1]))     # Ada is through

  ps <- sl_prediction_status(con, sid)
  expect_true(ps$has_questions)
  expect_equal(ps$n_done, 1)
  expect_setequal(ps$pending, c("Bo", "Cy", "Di"))

  sl_force_predictions(con, sid)
  expect_false(sl_player_predicts_pending(con, sl_session(con, sid), g, ids[2]))

  # predictions can't be set once the game moves past round 1
  for (pid in ids) sl_submit(con, sid, pid, 1, 20)
  sl_score_round(con, sid); sl_advance_round(con, sid)
  expect_error(sl_set_prediction(con, sid, ids[1], "own_r1", "9"), "before round 1")
})

test_that("a session can skip the prediction questions", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH, ask_predictions = FALSE)
  ids <- join(con, sid, c("Ada", "Bo", "Cy", "Di"))
  sl_start_game(con, sid, seed = 1)
  s <- sl_session(con, sid); g <- sl_game(s)
  expect_gt(length(game_required_qids(g)), 0)                  # the game has questions...
  expect_false(sl_player_predicts_pending(con, s, g, ids[1]))  # ...but nobody is held back
  expect_false(sl_prediction_status(con, sid)$has_questions)   # and the admin note is hidden
  # the default still asks, as before
  expect_equal(sl_session(con, sl_create_session(con, "bertrand", GAMES_PATH))$ask_predictions, 1)
})

test_that("confirm is sticky and round status separates priced vs confirmed", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("a", "b", "c", "d"))
  sl_start_game(con, sid, seed = 1)

  sl_submit(con, sid, ids[1], 1, 30)                       # auto-save, not confirmed
  sl_submit(con, sid, ids[2], 1, 40, confirm = TRUE)       # confirmed
  rs <- sl_round_status(con, sid)
  expect_equal(rs$n_priced, 2)
  expect_equal(rs$n_confirmed, 1)
  expect_true("a" %in% rs$unconfirmed && !("b" %in% rs$unconfirmed))

  sl_submit(con, sid, ids[1], 1, 25, confirm = TRUE)       # a confirms
  sl_submit(con, sid, ids[1], 1, 22)                       # ...then edits again (no confirm arg)
  row <- db_one(con, "SELECT price, confirmed FROM submissions WHERE session_id=? AND round=1 AND player_id=?",
                list(sid, ids[1]))
  expect_equal(row$price, 22)                              # price updated
  expect_equal(row$confirmed, 1)                           # confirm stays sticky
})

test_that("anonymous + projector-view settings round-trip", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  expect_equal(sl_session(con, sid)$anonymous, 0)
  sl_set_anonymous(con, sid, TRUE)
  expect_equal(sl_session(con, sid)$anonymous, 1)

  expect_equal(sl_session(con, sid)$projector_view, "summary")   # default
  sl_set_projector(con, sid, "detail")
  expect_equal(sl_session(con, sid)$projector_view, "detail")
  expect_error(sl_set_projector(con, sid, "nonsense"))
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
