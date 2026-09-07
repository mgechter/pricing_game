# Debrief data + plots (shared by the Shiny tab and debrief.qmd).

suppressMessages(library(ggplot2))

test_that(".longest_true_run finds the longest consecutive TRUE block", {
  r <- .longest_true_run(c(F, T, T, F, T, T, T, T, F))
  expect_equal(r$len, 4); expect_equal(r$start, 5); expect_equal(r$end, 8)
  expect_equal(.longest_true_run(c(F, F))$len, 0)
})

test_that("dbf_load returns the expected shape", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, 120)

  d <- dbf_load(con, sid)
  expect_equal(d$model, "hotelling_linear")
  expect_equal(d$benchmarks$ne, 100)
  expect_true(nrow(d$results) > 0)
  expect_true(all(c("round", "market_id", "firm_label", "price", "profit") %in% names(d$results)))
})

test_that("dbf_flags catches tacit collusion and an off-best-response move", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)   # NE = 0
  join(con, sid, c("A", "B")); sl_start_game(con, sid, seed = 1)
  # both firms hold well above NE every round -> a collusion run;
  # in round 2, Firm A jumps to a wild price (far from its best response of ~just under 25)
  play_out(con, sid, function(round, firm, pid) {
    if (round == 2 && firm == "Firm A") 95 else 25
  })
  fr <- dbf_flags(dbf_load(con, sid))
  expect_true("Tacit collusion" %in% fr$kind)
  expect_true("Off best response" %in% fr$kind)
})

test_that("dbf_leaderboards totals and vs-NE line up", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 3)
  play_out(con, sid, 150)

  d  <- dbf_load(con, sid)
  lb <- dbf_leaderboards(d)
  res <- d$results
  for (i in seq_len(nrow(lb$individuals))) {
    nm <- lb$individuals$display_name[i]
    expect_equal(lb$individuals$total_profit[i],
                 sum(res$profit[res$display_name == nm]))
  }
  nr <- d$session$n_rounds
  expect_equal(lb$individuals$vs_ne,
               lb$individuals$total_profit - nr * lb$ne_profit_per_round)
})

test_that("plot builders return ggplot objects; best-response guards bad input", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) 90 + 10 * r)
  d <- dbf_load(con, sid)

  expect_s3_class(plot_price_paths(d), "ggplot")
  expect_s3_class(plot_price_paths(d, anon = TRUE), "ggplot")
  expect_s3_class(plot_avg_price(d), "ggplot")
  expect_s3_class(plot_profit_paths(d), "ggplot")
  expect_s3_class(plot_best_response(d, min(d$results$market_id), 2, "Firm A"), "ggplot")
  expect_error(plot_best_response(d, 999, 2, "Firm A"), "isn't available")
})

test_that("debrief_write_figs writes the standard PNG set", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:6)); sl_start_game(con, sid, seed = 2)
  play_out(con, sid, function(r, f, p) round(runif(1, 60, 220)))
  d   <- dbf_load(con, sid)
  dir <- tempfile("figs_")
  files <- debrief_write_figs(d, dir)
  expect_true(length(files) >= 4)
  expect_true(all(file.exists(files)))
  expect_true(any(grepl("price_paths\\.png$", files)))
})

test_that("dbf_load_bundle round-trips through the export files", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, 130)

  dir <- tempfile("bundle_"); dir.create(dir)
  b <- sl_export_bundle(con, sid)
  for (nm in names(b)) utils::write.csv(b[[nm]], file.path(dir, paste0(nm, ".csv")),
                                        row.names = FALSE, na = "")
  db <- dbf_load_bundle(dir)
  live <- dbf_load(con, sid)
  expect_equal(db$benchmarks$ne, live$benchmarks$ne)
  expect_equal(nrow(db$results), nrow(live$results))
  expect_s3_class(plot_price_paths(db), "ggplot")
})
