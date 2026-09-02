# Game-level resolution and config loading.

test_that("resolve_game accumulates profit within (market_id, firm)", {
  game <- list(demand = list(model = "bertrand_homogeneous",
                             params = list(choke = 100, mc = 0)))
  sub <- data.frame(
    firm      = c("A", "B", "A", "B"),
    market_id = c(1, 1, 1, 1),
    round     = c(1, 1, 2, 2),
    price     = c(40, 40, 30, 50),
    stringsAsFactors = FALSE
  )
  r <- resolve_game(game, sub)
  a <- r[r$firm == "A", ]
  expect_equal(a$profit, c(30 * 40, 70 * 30))       # tie split, then wins outright
  expect_equal(a$cumulative_profit, cumsum(a$profit))
  b <- r[r$firm == "B", ]
  expect_equal(b$cumulative_profit, c(1200, 1200))
})

test_that("resolve_game warns and skips a market-round without exactly 2 firms", {
  game <- list(demand = list(model = "bertrand_homogeneous",
                             params = list(choke = 100, mc = 0)))
  sub <- data.frame(firm = c("A", "B", "A"), market_id = c(1, 1, 2),
                    round = c(1, 1, 1), price = c(40, 40, 20),
                    stringsAsFactors = FALSE)
  expect_warning(r <- resolve_game(game, sub), "skipping")
  expect_equal(nrow(r), 2L)
})

test_that("games.yaml loads with unique ids and valid models", {
  g <- load_games(GAMES_PATH)
  expect_setequal(names(g), c("bertrand", "hotelling_high", "hotelling_low"))
  for (x in g) {
    expect_true(all(c("id", "rounds", "pairing", "demand", "decisions") %in% names(x)))
    expect_true(x$demand$model %in% names(DEMAND_MODELS))
    expect_true(is.numeric(x$rounds) && x$rounds >= 1)
  }
})
