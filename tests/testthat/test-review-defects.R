# Defects found by an adversarial review of the fork (Codex, 2026-11-18). Each
# test failed at 47bd021 and passes with its fix; the IDs follow the review's
# report. Tests that need the class-only Slide figures tab are in
# test-slide-figures.R.

for (.f in c("ui_helpers.R", "mod_admin.R", "mod_player.R", "mod_display.R"))
  source(file.path(root, "R", .f))
# admin_server calls the class-only Slide figures tab's server where it exists
if (file.exists(file.path(root, "R", "slide_figures.R")))
  source(file.path(root, "R", "slide_figures.R"))

.cx_start <- function(con, game = "bertrand", n = 2L) {
  id <- sl_create_session(con, game, GAMES_PATH, ask_predictions = FALSE)
  join(con, id, paste0("Student ", seq_len(n)))
  sl_start_game(con, id, seed = 1)
  id
}
.cx_score <- function(con, id, price = 20) {
  pl <- sl_players(con, id)
  r <- sl_session(con, id)$current_round
  for (i in seq_len(nrow(pl))) {
    p <- if (is.function(price)) price(pl[i, ], r) else price
    if (!is.na(p)) sl_submit(con, id, pl$player_id[i], r, p)
  }
  sl_score_round(con, id)
}

# --- the student page, scoring and the projector ------------------------------

test_that("C1: a price box with no value (NULL) must not close a student's session", {
  con <- fresh_db(); csid <- .cx_start(con)
  shiny::testServer(player_server, args = list(con = con), {
    token(sl_players(con, csid)$token[1])
    session$elapse(1100)
    session$setInputs(price = 20, rationale = "")
    session$elapse(1300)
    session$setInputs(price = NULL, rationale = "")
    suppressWarnings(session$elapse(1300))
    expect_false(session$isClosed())
  })
})

test_that("C1: a cleared price box (NA, as Shiny delivers it) keeps the saved price", {
  con <- fresh_db(); csid <- .cx_start(con)
  pid <- sl_players(con, csid)$player_id[1]
  shiny::testServer(player_server, args = list(con = con), {
    token(sl_players(con, csid)$token[1])
    session$elapse(1100)
    session$setInputs(price = 20, rationale = "")
    session$elapse(1300)
    session$setInputs(price = NA, rationale = "")
    session$elapse(1300)
    expect_false(session$isClosed())
    sub <- db_query(con, "SELECT price FROM submissions WHERE session_id = ? AND player_id = ?",
                    list(csid, pid))
    expect_equal(sub$price, 20)
  })
})

test_that("C1: the timer's last seconds must not close the page of a student whose price box never appeared", {
  con <- fresh_db(); csid <- .cx_start(con)
  sl_set_timer(con, csid, 3)                      # a 3-second round: inside the last 5 s
  shiny::testServer(player_server, args = list(con = con), {
    token(sl_players(con, csid)$token[1])        # no price input has ever been sent
    suppressWarnings(session$elapse(2200))
    expect_false(session$isClosed())
  })
})

test_that("C2: carry-forward must tolerate a previous round with no saved price", {
  con <- fresh_db(); csid <- .cx_start(con)
  .cx_score(con, csid, function(p, r) if (p$firm_label == "Firm A") 20 else NA_real_)
  sl_advance_round(con, csid)
  # Firm B never submitted in round 1 and still has nothing to carry.
  a <- sl_players(con, csid); a <- a$player_id[a$firm_label == "Firm A"]
  sl_submit(con, csid, a, 2, 20)
  expect_error(sl_score_round(con, csid, carry_forward = TRUE), NA)
  r2 <- dbf_load(con, csid)$results
  r2 <- r2[r2$round == 2, ]
  expect_equal(r2$no_submission[r2$firm_label == "Firm B"], 1)     # scored as a no-show
  expect_equal(r2$profit[r2$firm_label == "Firm A"], 20 * 80)      # sole seller at $20
})

test_that("C3: a debounced round-one draft must not become a round-two submission", {
  con <- fresh_db(); csid <- .cx_start(con)
  shiny::testServer(player_server, args = list(con = con), {
    token(sl_players(con, csid)$token[1])
    session$elapse(1100)
    session$setInputs(price = 20, rationale = "round one draft")
    # Close/advance while the 1.2-second debounce is pending. The phone has
    # not yet acknowledged the replacement input sent by the 1-second poll.
    sl_score_round(con, csid); sl_advance_round(con, csid)
    session$elapse(1100); session$elapse(200)
    unexpected <- db_query(con,
      "SELECT * FROM submissions WHERE session_id = ? AND round = 2", list(csid))
    expect_equal(nrow(unexpected), 0L)
  })
})

test_that("C3: a draft typed in the current round is still saved", {
  con <- fresh_db(); csid <- .cx_start(con)
  shiny::testServer(player_server, args = list(con = con), {
    token(sl_players(con, csid)$token[1])
    session$elapse(1100)
    session$setInputs(price = 33, rationale = "mine")
    session$elapse(1300)
    sub <- db_query(con, "SELECT price, rationale FROM submissions WHERE session_id = ? AND round = 1",
                    list(csid))
    expect_equal(sub$price, 33); expect_equal(sub$rationale, "mine")
  })
})

test_that("C4: the projector must refresh profits after a carry-forward re-score", {
  con <- fresh_db(); csid <- .cx_start(con)
  .cx_score(con, csid); sl_advance_round(con, csid)
  .cx_score(con, csid, function(p, r) if (p$firm_label == "Firm A") 20 else NA_real_)
  shiny::testServer(display_server, args = list(con = con), {
    session$elapse(1100)
    before <- d()$results  # populate the projector cache
    sl_score_round(con, csid, carry_forward = TRUE)
    session$elapse(1100)
    expected <- dbf_load(con, csid)$results
    expect_equal(d()$results$profit, expected$profit)
  })
})

test_that("C5: the round-one timer must work after an early end and a reset", {
  con <- fresh_db(); csid <- .cx_start(con)
  sl_set_timer(con, csid, 1)
  expire <- function() db_run(con,
    "UPDATE sessions SET round_started_at = ? WHERE session_id = ?",
    list(now_epoch() - 10, csid))
  expire()
  shiny::testServer(admin_server, args = list(con = con, games_path = GAMES_PATH), {
    session$elapse(1100)
    stopifnot(sl_session(con, csid)$phase == "scored")
    sl_end_early(con, csid); sl_reset_to_lobby(con, csid)
    session$elapse(1100)
    sl_start_game(con, csid, seed = 1); expire()
    session$elapse(2200)
    expect_equal(sl_session(con, csid)$phase, "scored")
  })
})
