# Benchmark prices and best responses.

dem <- function(id) load_games(GAMES_PATH)[[id]]$demand

test_that("NE prices are mc + t*L (Hotelling) and mc (Bertrand)", {
  expect_equal(ne_price(dem("hotelling_high")$model, dem("hotelling_high")$params), 100)
  expect_equal(ne_price(dem("hotelling_low")$model,  dem("hotelling_low")$params),  50)
  expect_equal(ne_price(dem("bertrand")$model,       dem("bertrand")$params),       0)
})

test_that("monopoly prices match the benchmark the 2026 slides show", {
  expect_equal(monopoly_price(dem("hotelling_high")$model, dem("hotelling_high")$params), 150)
  expect_equal(monopoly_price(dem("hotelling_low")$model,  dem("hotelling_low")$params),  175)
  expect_equal(monopoly_price(dem("bertrand")$model,       dem("bertrand")$params),        50)
})

test_that("resolve_benchmarks turns 'compute' into numbers (numeric by default)", {
  g <- resolve_benchmarks(load_games(GAMES_PATH)[["hotelling_high"]])
  expect_equal(g$benchmarks$ne, 100, tolerance = 0.05)          # numeric fixed point
  expect_equal(g$benchmarks$monopoly, 150)                      # closed-form reference
  # coverage_decay is on for the rebuilt games, so collusive == monopoly
  expect_equal(g$benchmarks$collusive, 200, tolerance = 0.5)    # numeric joint-profit max
})

test_that("resolve_benchmarks(method = 'closed') returns the exact closed forms", {
  g <- resolve_benchmarks(load_games(GAMES_PATH)[["hotelling_low"]], method = "closed")
  expect_equal(g$benchmarks$ne, 50)
  expect_equal(g$benchmarks$monopoly, 175)
  expect_equal(g$benchmarks$collusive, 175)                     # coverage_decay on
})

test_that("grid best response matches the closed form in the Hotelling interior", {
  p  <- list(V = 250, t = 1, L = 100, M = 100, mc = 0)
  br <- best_response("hotelling_linear", p, rival_price = 180, by = 0.5)
  expect_equal(br$price, best_response_interior_hotelling(p, 180), tolerance = 0.5)  # 140
  expect_equal(nrow(br$curve), length(seq(0, 250, by = 0.5)))
})

test_that("Bertrand best response undercuts, capped at the monopoly price", {
  p <- list(choke = 100, mc = 0)
  expect_lt(best_response("bertrand_homogeneous", p, rival_price = 30, by = 0.5)$price, 30)
  expect_equal(best_response("bertrand_homogeneous", p, rival_price = 90, by = 0.5)$price,
               50, tolerance = 0.5)
})
