# Parity against the instructor's hand-built all_profits.csv (2026).
# See docs/phase0-parity-findings.md.

test_that("2026 profit totals reproduce to the cent for all three games", {
  cmp <- run_parity(DATA_DIR, GAMES_PATH)          # applies the O3 = 2 sheet overrides
  expect_false(anyNA(cmp$computed))                # every player name matched
  # all_profits.csv is rounded to whole dollars, so <= $0.50 per player total
  expect_lte(max(abs(cmp$diff)), 0.5 + 1e-9)
})

test_that("the 2026 sheet ran the same parameters in both differentiated games", {
  # hotelling_high and hotelling_low both reproduce only with t (O3) = 2; the
  # 'high vs low differentiation' contrast did not exist in the 2026 numbers.
  expect_equal(.sheet_2026$hotelling_high$t, 2)
  expect_equal(.sheet_2026$hotelling_low$t,  2)
  cmp <- run_parity(DATA_DIR, GAMES_PATH)
  for (g in c("hotelling_high", "hotelling_low")) {
    expect_lte(max(abs(cmp$diff[cmp$game_id == g])), 0.5 + 1e-9)
  }
})

test_that("as-designed run (t = 1 / t = 0.5) differs from the 2026 sheet", {
  designed <- run_parity(DATA_DIR, GAMES_PATH, as_designed = TRUE)
  h <- designed[designed$game_id != "bertrand", ]
  expect_gt(max(abs(h$pct_diff)), 10)             # the intended games are NOT what was played
  # Bertrand is unaffected by the Hotelling params
  b <- designed[designed$game_id == "bertrand", ]
  expect_true(all(abs(b$diff) < 1))
})
