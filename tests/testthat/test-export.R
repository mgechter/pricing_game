# The analysis export bundle.

make_scored <- function(con, game = "bertrand", players = c("A", "B", "C", "D"),
                        label = NULL, price = 20) {
  sid <- sl_create_session(con, game, GAMES_PATH, label = label)
  join(con, sid, players)
  sl_start_game(con, sid, seed = 1)
  play_out(con, sid, price)
  sid
}

test_that("sl_export_bundle produces the four tidy tables", {
  con <- fresh_db()
  sid <- make_scored(con, "bertrand", label = "Sec A")
  b <- sl_export_bundle(con, sid)

  expect_setequal(names(b), c("results_long", "submissions", "pairs", "benchmarks"))
  nr <- sl_session(con, sid)$n_rounds
  expect_equal(nrow(b$results_long), 4 * nr)                 # 4 players x rounds
  expect_true(all(c("session_id", "game_id", "game_label", "round", "market_id",
                    "firm_label", "display_name", "price", "profit",
                    "cumulative_profit", "no_submission") %in% names(b$results_long)))
  expect_equal(nrow(b$pairs), 4)
  expect_equal(nrow(b$benchmarks), 1)
  expect_equal(b$benchmarks$game_id, "bertrand")
  expect_equal(b$benchmarks$ne, 0)
  expect_equal(b$benchmarks$monopoly, 50)
})

test_that("the pairs table names each shadow's host", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("A", "B", "C"))
  sl_start_game(con, sid, seed = 1)
  play_out(con, sid, 20)
  pr <- sl_export_bundle(con, sid)$pairs
  expect_true("shadow_of" %in% names(pr))
  sh <- pr[pr$firm_label == "Shadow", ]
  expect_equal(nrow(sh), 1)
  expect_true(!is.na(sh$shadow_of) && nzchar(sh$shadow_of))   # host's display name
})

test_that("a multi-session bundle stacks and tags each session", {
  con <- fresh_db()
  s1 <- make_scored(con, "bertrand", label = "g1")
  s2 <- make_scored(con, "hotelling_high", label = "g2", price = 120)
  b  <- sl_export_bundle(con, c(s1, s2))

  expect_setequal(unique(b$results_long$session_id), c(s1, s2))
  expect_setequal(unique(b$benchmarks$game_id), c("bertrand", "hotelling_high"))
  expect_equal(nrow(b$benchmarks), 2)
})

test_that("bundle writes to a valid ZIP and to xlsx", {
  con <- fresh_db()
  sid <- make_scored(con)
  b   <- sl_export_bundle(con, sid)

  z <- write_bundle_zip(b, tempfile(fileext = ".zip"))
  expect_true(file.exists(z))
  entries <- zip::zip_list(z)$filename
  expect_true(all(c("results_long.csv", "submissions.csv", "pairs.csv",
                    "benchmarks.csv", "README.txt") %in% entries))

  x <- write_bundle_xlsx(b, tempfile(fileext = ".xlsx"))
  expect_true(file.exists(x) && file.size(x) > 0)
})
