# Guards for the attacks that held in an adversarial review of the fork (Codex,
# 2026-11-18): each passed at 47bd021 and should keep passing. The IDs follow
# the review's report. Several use the class-only Slide figures tab, so this
# file is ECON 30303's, not for upstream.
for (.cg_file in c("ui_helpers.R", "slide_figures.R", "mod_admin.R",
                   "mod_player.R", "mod_display.R"))
  source(file.path(root, "R", .cg_file))

.cg_start <- function(con, game = "bertrand", n = 2L, names = NULL,
                      ask_predictions = FALSE) {
  id <- sl_create_session(con, game, GAMES_PATH, ask_predictions = ask_predictions)
  join(con, id, names %||% paste0("Guard student ", seq_len(n)))
  sl_start_game(con, id, seed = 1)
  id
}
.cg_score <- function(con, id, price = 20) {
  pl <- sl_players(con, id); r <- sl_session(con, id)$current_round
  for (i in seq_len(nrow(pl))) {
    p <- if (is.function(price)) price(pl[i, ], r) else price
    if (!is.na(p)) sl_submit(con, id, pl$player_id[i], r, p)
  }
  sl_score_round(con, id)
}

test_that("G1: duplicate names are rejected and unusual names retain identity and HTML escaping", {
  withr::local_pdf(tempfile(fileext = ".pdf"))
  con <- fresh_db()
  csid <- sl_create_session(con, "bertrand", GAMES_PATH)
  tricky <- c('O\'Brien, "A" | B', '<b>Alice</b> & Co', 'Student \U0001f642',
              paste(rep("VeryLongName", 30), collapse = ""))
  ids <- join(con, csid, tricky)
  expect_error(sl_add_player(con, csid, tricky[1]), "already taken")
  expect_error(sl_add_player(con, csid, toupper(tricky[1])), "already taken")
  expect_equal(sl_players(con, csid)$display_name, tricky)
  expect_length(unique(ids), 4)
  html <- as.character(html_table(data.frame(Name = tricky)))
  expect_match(html, "&lt;b&gt;Alice&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>Alice</b>", html, fixed = TRUE))
  sl_start_game(con, csid, seed = 1); .cg_score(con, csid); sl_end_early(con, csid)
  ds <- slide_figures_data(con)
  expect_equal(unname(unlist(slide_market_choices(ds))), c("bertrand|1", "bertrand|2"))
  for (mk in 1:2) {
    one <- slide_market_figure(ds, paste0("bertrand|", mk))
    expect_match(one$id, paste0("_market", mk, "$"))
    expect_s3_class(suppressWarnings(ggplot2::ggplotGrob(one$plot)), "gtable")
  }
})

test_that("G2: Slide figures choose the latest scored session and fall back after reset or delete", {
  con <- fresh_db()
  old <- .cg_start(con); .cg_score(con, old, 20); sl_end_early(con, old)
  fresh <- .cg_start(con)
  # An open round with submissions alone must not displace the older game.
  for (pid in sl_players(con, fresh)$player_id) sl_submit(con, fresh, pid, 1, 40)
  expect_equal(slide_figures_data(con)$bertrand$session$session_id, old)
  sl_score_round(con, fresh)
  expect_equal(slide_figures_data(con)$bertrand$session$session_id, fresh)
  sl_advance_round(con, fresh)
  for (pid in sl_players(con, fresh)$player_id) sl_submit(con, fresh, pid, 2, 99)
  d <- slide_figures_data(con)$bertrand
  expect_equal(unique(d$results$price), 40)
  expect_equal(unique(d$results$round), 1L)
  expect_match(slide_figures_build(con)$bertrand_price_paths$source, "game still running")
  sl_reset_to_lobby(con, fresh)
  expect_equal(slide_figures_data(con)$bertrand$session$session_id, old)
  sl_delete_session(con, old)
  expect_null(slide_figures_data(con)$bertrand)
})

test_that("G3: a one-round early end has correct slide provenance and reset restores the configured horizon", {
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_low", 3)
  tokens <- sl_players(con, csid)$token
  .cg_score(con, csid, 150); sl_end_early(con, csid)
  figs <- slide_figures_build(con)
  expect_equal(sl_session(con, csid)$n_rounds, 1L)
  expect_identical(figs$hotelling_low_price_paths$source,
                   sprintf("Session #%d: 1 round scored.", csid))
  expect_false(grepl("NA", figs$hotelling_low_price_paths$source, fixed = TRUE))
  expect_null(figs$hotelling_low_last_round_cuts$plot)
  expect_match(figs$hotelling_low_last_round_cuts$note, "one round")
  expect_equal(dbf_leaderboards(dbf_load(con, csid))$rounds, 1L)
  sl_reset_to_lobby(con, csid)
  expect_equal(sl_session(con, csid)$n_rounds, 10L)
  expect_equal(sl_players(con, csid)$token, tokens)
  expect_equal(nrow(dbf_load(con, csid)$results), 0L)
  expect_null(slide_figures_data(con)$hotelling_low)
})

test_that("G4: new market ranks sum only Firm A plus Firm B even when the shadow is more profitable", {
  con <- fresh_db(); csid <- .cg_start(con, n = 5)
  .cg_score(con, csid, function(p, r) if (p$firm_label == "Shadow") 49 else 80)
  sl_end_early(con, csid)
  d <- dbf_load(con, csid)
  rank <- dbf_market_ranks(d)
  # Each real firm gets (100 - 80)/2 * 80 = 800. Shadow gets 49*51 = 2499.
  expect_equal(rank$joint_profit, c(1600, 1600))
  expect_equal(rank$market_id, 1:2)
  expect_equal(rank$rank, 1:2)
  expect_equal(d$results$profit[d$results$firm_label == "Shadow"], 2499)
  expect_match(plot_market_detail(d, 1)$labels$subtitle, "$1,600.00", fixed = TRUE)
})

test_that("G5: price quartiles include actual shadow and carried prices and exclude missing and unscored prices", {
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_high", 5)
  pl <- sl_players(con, csid)
  # Explicit sample: 0, 10, 20, 30, NA -> Q25=7.5, median=15, Q75=22.5.
  ps <- c(0, 10, 20, 30, NA_real_)
  .cg_score(con, csid, function(p, r) ps[match(p$player_id, pl$player_id)])
  q1 <- plot_price_quartiles(dbf_load(con, csid))$data
  expect_equal(q1$price, c(22.5, 15, 7.5))
  sl_advance_round(con, csid)
  # Student 5 now prices; every other student's finite price is carried.
  sl_submit(con, csid, pl$player_id[5], 2, 40)
  sl_score_round(con, csid, carry_forward = TRUE)
  sl_advance_round(con, csid)
  sl_submit(con, csid, pl$player_id[1], 3, 1000)
  d <- dbf_load(con, csid)
  q2 <- plot_price_quartiles(d)$data
  expect_equal(q2$price[q2$round == 2], c(30, 20, 10))
  expect_setequal(q2$round, 1:2)
  expect_equal(sum(d$submissions$carried), 4L)
})

test_that("G6: equal-price quartiles retain the price and render in a one-round game", {
  withr::local_pdf(tempfile(fileext = ".pdf"))
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_low", 5)
  .cg_score(con, csid, 0); sl_end_early(con, csid)
  p <- plot_price_quartiles(dbf_load(con, csid))
  expect_equal(p$data$price, c(0, 0, 0))
  expect_s3_class(suppressMessages(ggplot2::ggplotGrob(p)), "gtable")
})

test_that("G7: rival finders identify the correct firm and ignore shadows and missing rivals", {
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_low", 5)
  .cg_score(con, csid, function(p, r) {
    if (p$firm_label == "Shadow") 10000
    else if (p$market_id == 1) if (p$firm_label == "Firm A") 80 else 240
    else if (p$firm_label == "Firm A") 0 else NA_real_
  })
  sl_end_early(con, csid); d <- dbf_load(con, csid)
  cs <- dbf_rival_case(d, "above_monopoly")
  expect_equal(cs, list(market_id = 1L, round = 1L, firm_label = "Firm A",
                        price = 80, rival_price = 240, exact = TRUE))
  below <- dbf_rival_case(d, "below_ne")
  expect_equal(below$market_id, 1L)
  expect_equal(below$firm_label, "Firm B")
  expect_false(below$exact)
  flags <- dbf_flags(d)
  hi <- flags[flags$kind == "Rival above monopoly", ]
  expect_equal(hi$firm_label, "Firm A")
  expect_equal(hi$market_id, 1L)
  expect_equal(hi$score, 15)
  f <- slide_figures_build(con)$hotelling_low_rival_above_monopoly_both_firms
  expect_match(f$note, "Market 1, round 1", fixed = TRUE)
  expect_match(f$note, "$240.00", fixed = TRUE)
})

test_that("G8: last-round cuts align firms and use the last scored round after an early end", {
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_low", 5)
  .cg_score(con, csid, function(p, r) if (p$firm_label == "Shadow") 1000 else 200)
  sl_advance_round(con, csid)
  .cg_score(con, csid, function(p, r) {
    if (p$firm_label == "Shadow") 0 else if (p$market_id == 1) 150 else 190
  })
  sl_end_early(con, csid); d <- dbf_load(con, csid)
  # Reverse result order to ensure the calculation matches firms, not row slots.
  d$results <- d$results[rev(seq_len(nrow(d$results))), ]
  expect_equal(dbf_last_round_cuts(d),
    data.frame(market_id = 1:2, cut = c(50, 10), exact = c(TRUE, TRUE)))
  expect_match(slide_figures_build(con)$hotelling_low_last_round_cuts$note,
               "In round 2, the last round played", fixed = TRUE)
})

test_that("G9: valid carry-forward scoring and shadow-host matching preserve the numerical outcomes", {
  con <- fresh_db(); csid <- .cg_start(con, n = 3)
  .cg_score(con, csid, 20); sl_advance_round(con, csid)
  sl_score_round(con, csid, carry_forward = TRUE)
  d <- dbf_load(con, csid)
  expect_equal(d$results$profit, rep(800, 6))
  expect_equal(d$results$cumulative_profit[d$results$round == 2], rep(1600, 3))
  expect_equal(d$results$rival_price, rep(20, 6))
  expect_equal(d$results$no_submission, rep(0L, 6))
  sl_advance_round(con, csid)
  pl <- sl_players(con, csid); shadow <- pl[pl$firm_label == "Shadow", ]
  # The host no-shows; only the shadow submits. It now faces a sole-seller case.
  sl_submit(con, csid, shadow$player_id, 3, 20)
  sl_score_round(con, csid)
  sh <- dbf_load(con, csid)$results
  sh <- sh[sh$firm_label == "Shadow" & sh$round == 3, ]
  expect_true(is.na(sh$rival_price))
  expect_equal(sh$profit, 1600)
})

test_that("G10: no-shows and boundary prices score correctly in all three configured games", {
  for (game in c("bertrand", "hotelling_high", "hotelling_low")) {
    con <- fresh_db(); csid <- .cg_start(con, game)
    .cg_score(con, csid, NA_real_)
    d <- dbf_load(con, csid)
    expect_equal(d$results$profit, c(0, 0))
    expect_true(all(is.na(d$results$price)))
    expect_equal(d$results$no_submission, c(1L, 1L))
    sl_advance_round(con, csid)
    edge <- if (game == "bertrand") 100 else 250
    .cg_score(con, csid, function(p, r) if (p$firm_label == "Firm A") edge else edge + 50)
    d <- dbf_load(con, csid)
    expect_equal(d$results$profit[d$results$round == 2], c(0, 0))
    sl_advance_round(con, csid); .cg_score(con, csid, 0)
    d <- dbf_load(con, csid)
    expect_equal(d$results$profit[d$results$round == 3], c(0, 0))
    expect_equal(d$results$quantity[d$results$round == 3], c(50, 50))
  }
})

test_that("G11: Hotelling best responses agree with an independent analytic piecewise optimum", {
  for (game in c("hotelling_high", "hotelling_low")) {
    g <- get_game(game, GAMES_PATH); p <- g$demand$params
    a <- p$t * p$L
    for (rival in c(0, 25, 100, 200, 225, 250, 300)) {
      # Demand changes slope at full capture and at the uncovered-market
      # boundary. Check these kinks plus the stationary points of each branch.
      candidates <- unique(pmax(0, pmin(p$V,
        c(0, p$V, rival - a, 2 * p$V - a - rival,
          (a + rival) / 2, p$V / 2, p$V - a))))
      profit <- function(x) x * 100 * pmax(0, pmin(1,
        .5 + (rival - x)/(2*a), (p$V-x)/a))
      expected <- max(profit(candidates))
      br <- best_response(g$demand$model, p, rival)
      expect_equal(profit(br$price), expected, tolerance = .00002)
    }
  }
})

test_that("G12: anonymous plot labels and paired best-response panels retain the correct firm and round", {
  withr::local_pdf(tempfile(fileext = ".pdf"))
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_low", 3)
  .cg_score(con, csid, function(p, r) if (p$firm_label == "Firm A") 80 else 240)
  sl_end_early(con, csid); d <- dbf_load(con, csid)
  p <- plot_best_response_pair(d, 1, 1, first = "Firm B", anon = TRUE)
  panels <- levels(p$data$panel)
  expect_match(panels[1], "^Firm B, given Firm A's price of \\$80.00")
  expect_match(panels[2], "^Firm A, given Firm B's price of \\$240.00")
  expect_match(p$labels$title, "Market 1, round 1", fixed = TRUE)
  expect_false(any(grepl("Guard student", panels, fixed = TRUE)))
  expect_equal(levels(plot_price_paths(d, anon = TRUE)$data$market), "Market 1")
  expect_equal(plot_market_detail(d, 1, anon = TRUE)$labels$title, "Market 1")
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("G13: Slide ZIP contains every available figure and market with unique correct paths", {
  con <- fresh_db()
  for (g in c("bertrand", "hotelling_high", "hotelling_low")) {
    csid <- .cg_start(con, g, 3)
    .cg_score(con, csid, if (g == "bertrand") 60 else 240)
    sl_advance_round(con, csid); .cg_score(con, csid, 20)
    sl_end_early(con, csid)
  }
  ds <- slide_figures_data(con); figs <- slide_figures_from(ds, anon = TRUE)
  dir <- tempfile("codex_slide_guard_"); archive <- tempfile(fileext = ".zip")
  withr::defer(unlink(c(dir, archive), recursive = TRUE))
  files <- slide_figures_write(figs, dir, ds = ds, anon = TRUE)
  relative <- substring(files, nchar(dir) + 2)
  zip::zip(archive, files = relative, root = dir)
  expect_setequal(zip::zip_list(archive)$filename, relative)
  expect_equal(anyDuplicated(relative), 0L)
  expect_true(all(file.size(files) > 0))
  expect_setequal(relative[startsWith(relative, "markets/")],
    paste0("markets/", c("bertrand", "hotelling_high", "hotelling_low"), "_rank1_market1.png"))
  captions <- readLines(file.path(dir, "captions.txt"))
  for (d in ds) expect_true(any(grepl(paste0("Session #", d$session$session_id, ": 2 rounds scored."),
                                     captions, fixed = TRUE)))
  expect_false(any(grepl("Guard student|, NA", captions)))
})

test_that("G14: all-no-show Slide figures skip unavailable cases without breaking their ZIP", {
  con <- fresh_db(); csid <- .cg_start(con)
  sl_score_round(con, csid); sl_end_early(con, csid)
  ds <- slide_figures_data(con); figs <- slide_figures_from(ds)
  expect_null(figs$bertrand_rival_above_monopoly$plot)
  expect_null(figs$bertrand_price_quartiles$plot)
  dir <- tempfile("codex_noshow_slide_"); withr::defer(unlink(dir, recursive = TRUE))
  expect_error(files <- suppressMessages(slide_figures_write(figs, dir, ds)), NA)
  expect_true(all(file.size(files) > 0))
})

test_that("G15: optional predictions and the force button release every player including a shadow", {
  con <- fresh_db(); csid <- .cg_start(con, n = 3, ask_predictions = TRUE)
  s <- sl_session(con, csid); g <- sl_game(s); pids <- sl_players(con, csid)$player_id
  pending <- function() vapply(pids, function(p)
    sl_player_predicts_pending(con, sl_session(con, csid), g, p), logical(1))
  expect_true(all(pending()))
  for (q in game_predictions(g)) {
    if (!q$optional) sl_set_prediction(con, csid, pids[1], q$id,
                                      if (q$type == "numeric") "20" else "Yes")
  }
  expect_equal(pending(), c(FALSE, TRUE, TRUE))
  shiny::testServer(admin_server, args = list(con = con, games_path = GAMES_PATH), {
    session$setInputs(force_pred = 1)
    expect_false(any(pending()))
  })
  csid2 <- .cg_start(con, n = 3, ask_predictions = FALSE)
  s2 <- sl_session(con, csid2)
  expect_false(sl_prediction_status(con, csid2)$has_questions)
  expect_false(any(vapply(sl_players(con, csid2)$player_id, function(p)
    sl_player_predicts_pending(con, s2, sl_game(s2), p), logical(1))))
})

test_that("G16: lobby counts update for an already joined student and money keeps its cents", {
  con <- fresh_db(); csid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, csid, c("First", "Second"))
  shiny::testServer(player_server, args = list(con = con), {
    token(sl_players(con, csid)$token[1]); session$elapse(1100)
    expect_match(output$lobby_count$html, "2 player(s) joined.", fixed = TRUE)
    join(con, csid, "Third"); session$elapse(1100)
    expect_match(output$lobby_count$html, "3 player(s) joined.", fixed = TRUE)
  })
  expect_equal(money2(c(19.50, 0, NA)), c("$19.50", "$0.00", "—"))
  sl_start_game(con, csid, seed = 1); .cg_score(con, csid, 19.50)
  st <- sl_load_state(con, csid); p <- as.list(st$players[1, ])
  result <- st$results[st$results$player_id == p$player_id, ]
  expect_match(as.character(pl_result(p, st, result)), "$19.50", fixed = TRUE)
})

test_that("G17: serial writes by thirty students preserve every final price and confirmation", {
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_high", 30)
  pl <- sl_players(con, csid)
  for (r in 1:3) {
    for (edit in 1:3) for (i in seq_len(nrow(pl)))
      sl_submit(con, csid, pl$player_id[i], r, 100 + i + edit/100,
                paste("student", i, "round", r), confirm = edit == 2)
    status <- sl_round_status(con, csid)
    expect_equal(status$n_priced, 30L)
    expect_equal(status$n_confirmed, 30L)
    sl_score_round(con, csid)
    d <- dbf_load(con, csid)
    rows <- d$results[d$results$round == r, ]
    expect_equal(rows$price, 100 + match(rows$player_id, pl$player_id) + .03)
    expect_equal(nrow(rows), 30L)
    expect_false(anyNA(rows$profit))
    if (r < 3) sl_advance_round(con, csid)
  }
  expect_equal(nrow(dbf_load(con, csid)$results), 90L)
})

test_that("G18: the Debrief market pickers and Both firms download use their chosen data", {
  con <- fresh_db(); csid <- .cg_start(con, "hotelling_low", 5)
  .cg_score(con, csid, function(p, r) 50 + p$market_id * 20)
  sl_advance_round(con, csid); .cg_score(con, csid, 80); sl_end_early(con, csid)
  shiny::testServer(admin_server, args = list(con = con, games_path = GAMES_PATH), {
    session$setInputs(nav_debrief = 1, db_markets = "2", db_detail_market = "2",
                      br_market = "2", br_round = "2", br_firm = "Both firms", db_anon = TRUE)
    expect_equal(paths_markets(dbrief()), 2L)
    expect_equal(detail_market(dbrief()), 2L)
    plotted <- br_plot(dbrief(), 2)
    expect_match(plotted$labels$title, "Market 2, round 2", fixed = TRUE)
    expect_length(levels(plotted$data$panel), 2L)
    # Invoke the actual Shiny download handlers via the mock session.
    expect_true(file.size(output$dl_price) > 0)
    expect_true(file.size(output$dl_detail) > 0)
    expect_true(file.size(output$dl_br) > 0)
    session$setInputs(debrief_refresh = 1)
    expect_equal(paths_markets(dbrief()), 2L)
    expect_equal(detail_market(dbrief()), 2L)
  })
})
