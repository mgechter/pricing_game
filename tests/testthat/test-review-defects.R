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

# --- the Debrief tab ------------------------------------------------------------

test_that("C6: the Debrief market leaderboard must exclude shadow profit", {
  con <- fresh_db(); csid <- .cx_start(con, n = 3)
  .cx_score(con, csid); sl_end_early(con, csid)
  d <- dbf_load(con, csid)
  # At a Bertrand $20 tie, A and B earn $800 each; the shadow earns another
  # $800 in its separate counterfactual market, which must not count here.
  expect_equal(dbf_leaderboards(d)$markets$market_profit, 1600)
})

test_that("C7: the displayed Bertrand best-response price must beat a feasible cent undercut", {
  con <- fresh_db(); csid <- .cx_start(con)
  .cx_score(con, csid); sl_end_early(con, csid)
  d <- dbf_load(con, csid)
  p <- plot_best_response(d, 1, 1, "Firm A")
  marks <- p$layers[[4]]$data
  label <- marks$text[marks$kind == "Best response"]
  shown <- as.numeric(sub("Best response \\$", "", label))
  # The labelled $20.00 earned $800; $19.99 earns $1,599.3999.
  expect_gte(own_profit(d$model, d$params, shown, 20),
             own_profit(d$model, d$params, 19.99, 20))
  expect_equal(label, "Best response $19.99")
  fr <- dbf_flags(d)
  expect_false(any(grepl("best response of $20.00", fr$detail, fixed = TRUE)))
})

test_that("C7: the Bertrand best response is the monopoly price above it, a tie at a cent, cost at cost", {
  p <- list(choke = 100, mc = 0)
  br <- function(r) best_response("bertrand_homogeneous", p, r)$price
  expect_equal(br(52.10), 50)          # rival above the monopoly price
  expect_equal(br(50), 49.99)          # at it: a cent under
  expect_equal(br(0.01), 0.01)         # a tie at a cent beats $0 at cost
  expect_equal(br(0), 0)               # rival at cost: nothing to gain
  expect_equal(resolve_benchmarks(get_game("bertrand", GAMES_PATH))$benchmarks$ne, 0)
})

test_that("C8: a scored all-no-show game must still allow a Debrief figure-pack download", {
  con <- fresh_db(); csid <- .cx_start(con)
  sl_score_round(con, csid); sl_end_early(con, csid)
  dir <- tempfile("codex_empty_pack_")
  withr::defer(unlink(dir, recursive = TRUE))
  expect_error(suppressMessages(debrief_write_figs(dbf_load(con, csid), dir, dpi = 30)), NA)
  # the projector's average-price chart draws a sentence, not an error
  expect_s3_class(ggplot2::ggplot_build(plot_avg_price(dbf_load(con, csid))), "ggplot_built")
})

test_that("C9: reopening Debrief after reset and replay must use the replay's results", {
  con <- fresh_db(); csid <- .cx_start(con)
  .cx_score(con, csid, 20); sl_end_early(con, csid)
  suppressMessages(shiny::testServer(admin_server,
    args = list(con = con, games_path = GAMES_PATH), {
      session$setInputs(nav_debrief = 1)
      original <- dbrief()$results
      session$setInputs(nav_run = 1, reset_ok = 1)
      session$elapse(1100)
      sl_start_game(con, csid, seed = 1)
      .cx_score(con, csid, 40); sl_end_early(con, csid)
      session$elapse(1100)
      session$setInputs(nav_debrief = 2)
      expect_equal(dbrief()$results$price, dbf_load(con, csid)$results$price)
    }))
})

test_that("C10: Both firms in the Debrief explorer must handle a market with one no-show", {
  con <- fresh_db(); csid <- .cx_start(con, "hotelling_low")
  .cx_score(con, csid, function(p, r) if (p$firm_label == "Firm A") 80 else NA_real_)
  sl_end_early(con, csid)
  suppressMessages(shiny::testServer(admin_server,
    args = list(con = con, games_path = GAMES_PATH), {
      session$setInputs(nav_debrief = 1, br_firm = "Both firms", br_round = 1, br_market = 1)
      # A Shiny validation/no-data message is acceptable; an ordinary render
      # error is not. Test the actual renderPlot, not only its plot builder.
      expect_error(tryCatch(output$db_br, error = function(e) {
        if (inherits(e, "shiny.silent.error")) NULL else stop(e)
      }), NA)
      # the firm whose rival did not submit, on its own
      session$setInputs(br_firm = "Firm A")
      expect_error(output$db_br, NA)
    }))
})

test_that("C11: Refresh must preserve the Debrief's Show as Firm A / B choice", {
  con <- fresh_db(); csid <- .cx_start(con)
  play_out(con, csid, 20)
  shiny::testServer(admin_server, args = list(con = con, games_path = GAMES_PATH), {
    session$setInputs(nav_debrief = 1, db_anon = TRUE)
    session$setInputs(debrief_refresh = 1)
    html <- output$screen$html
    checkbox <- regmatches(html, regexpr('<input[^>]*db_anon[^>]*>', html))
    expect_match(checkbox, "checked")
  })
})

test_that("C12: in-progress profit comparisons must use the number of scored rounds", {
  con <- fresh_db(); csid <- .cx_start(con, "hotelling_high")
  .cx_score(con, csid, 100); sl_advance_round(con, csid)
  # Round 2 is open. Both firms played NE in the only round that has a result.
  expect_equal(dbf_leaderboards(dbf_load(con, csid))$individuals$vs_ne, c(0, 0))
})

test_that("C13: an unchanged equilibrium path must not be flagged as a profit swing", {
  con <- fresh_db(); csid <- .cx_start(con, "hotelling_high")
  play_out(con, csid, 100)
  flags <- dbf_flags(dbf_load(con, csid))
  expect_false(any(flags$kind == "Profit swing"))
})

test_that("C16: Show as Firm A / B must hide names in the Debrief tables as well as plots", {
  con <- fresh_db(); csid <- .cx_start(con)
  play_out(con, csid, 20)
  shiny::testServer(admin_server, args = list(con = con, games_path = GAMES_PATH), {
    session$setInputs(nav_debrief = 1, db_anon = TRUE)
    displayed <- paste(output$db_flags, output$db_lb_indiv, output$db_lb_mkt)
    expect_false(grepl("Student 1", displayed, fixed = TRUE))
    expect_match(displayed, "Market 1, Firm A", fixed = TRUE)
    session$setInputs(db_anon = FALSE)
    expect_match(paste(output$db_lb_indiv), "Student 1", fixed = TRUE)
  })
})
