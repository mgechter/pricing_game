# The numerical benchmark solvers must agree with the hand-derived closed forms
# on the models where a closed form exists.

hp  <- function(...) modifyList(list(V = 250, t = 1, L = 100, M = 100, mc = 0), list(...))
hpc <- function(...) hp(coverage_decay = TRUE, ...)
bp  <- function(...) modifyList(list(choke = 100, mc = 0), list(...))

test_that("numerical NE matches the closed form for Hotelling (both t, both coverage modes)", {
  for (t in c(1, 0.5, 2)) {
    expect_equal(ne_price_numeric("hotelling_linear", hp(t = t)),
                 ne_price("hotelling_linear", hp(t = t)), tolerance = 0.02)
    expect_equal(ne_price_numeric("hotelling_linear", hpc(t = t)),
                 ne_price("hotelling_linear", hpc(t = t)), tolerance = 0.02)  # NE unchanged
  }
})

test_that("numerical NE finds the Bertrand corner at marginal cost", {
  expect_equal(ne_price_numeric("bertrand_homogeneous", bp(mc = 0)), 0, tolerance = 1e-6)
  expect_equal(ne_price_numeric("bertrand_homogeneous", bp(mc = 12)), 12, tolerance = 1e-6)
})

test_that("numerical NE responds to marginal cost in Hotelling (mc + t*L)", {
  expect_equal(ne_price_numeric("hotelling_linear", hp(mc = 20)), 120, tolerance = 0.05)
})

test_that("collusive price = V without coverage decay, = V - t*L/2 with it", {
  # no decay: joint profit climbs to the reservation price
  expect_equal(collusive_price_numeric("hotelling_linear", hp()), 250, tolerance = 0.5)
  expect_gt(collusive_price_numeric("hotelling_linear", hp()),
            monopoly_price("hotelling_linear", hp()))

  # with decay: the market empties from the middle past the covered boundary, so
  # the joint optimum sits exactly on the textbook line
  for (t in c(1, 0.5)) {
    expect_equal(collusive_price_numeric("hotelling_linear", hpc(t = t)),
                 monopoly_price("hotelling_linear", hpc(t = t)), tolerance = 0.5)
  }
})

test_that("coverage decay: symmetric high prices leave the market partly unserved", {
  p <- hpc(t = 1)                       # covered boundary = V - t*L/2 = 200
  lo <- resolve_round("hotelling_linear", p,
                      data.frame(firm = c("A", "B"), price = c(150, 150)))
  hi <- resolve_round("hotelling_linear", p,
                      data.frame(firm = c("A", "B"), price = c(230, 230)))
  expect_equal(sum(lo$share), 1)        # below the boundary: fully served
  expect_lt(sum(hi$share), 1)           # above it: middle of the street buys nothing
  expect_equal(sum(hi$share), 2 * (250 - 230) / (1 * 100))   # 2*(V-p)/(t*L) = 0.4
})

test_that("numerical collusive price matches the closed form for Bertrand", {
  expect_equal(collusive_price_numeric("bertrand_homogeneous", bp()),
               monopoly_price("bertrand_homogeneous", bp()), tolerance = 0.2)   # 50
  expect_equal(collusive_price_numeric("bertrand_homogeneous", bp(mc = 20)),
               60, tolerance = 0.2)                                             # (100+20)/2
})

test_that("best_response agrees with the Hotelling closed form in the interior", {
  p <- hp(t = 1)
  for (pj in c(120, 160, 200)) {
    expect_equal(best_response("hotelling_linear", p, pj)$price,
                 best_response_interior_hotelling(p, pj), tolerance = 0.3)
  }
})

test_that("the whole benchmark set computes fast enough to do at session start", {
  games <- load_games(GAMES_PATH)
  el <- system.time(lapply(games, resolve_benchmarks))[["elapsed"]]
  expect_lt(el, 2)                                            # all 3 games, one-time
})
