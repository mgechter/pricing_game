# Demand-model behaviour.

hp <- function(...) modifyList(list(V = 250, t = 1, L = 100, M = 100, mc = 0), list(...))
two <- function(pa, pb) data.frame(firm = c("A", "B"), price = c(pa, pb),
                                   stringsAsFactors = FALSE)

# ---- Bertrand -----------------------------------------------------------------

test_that("bertrand: the lower price serves the whole market", {
  r <- resolve_round("bertrand_homogeneous", list(choke = 100, mc = 0), two(30, 50))
  expect_equal(r$quantity[r$firm == "A"], 70)
  expect_equal(r$quantity[r$firm == "B"], 0)
  expect_equal(r$profit[r$firm == "A"], 70 * 30)
})

test_that("bertrand: equal prices split the market 50/50", {
  r <- resolve_round("bertrand_homogeneous", list(choke = 100, mc = 0), two(40, 40))
  expect_equal(r$quantity, c(30, 30))
  expect_equal(r$share, c(0.5, 0.5))
})

test_that("bertrand: a price at or above the choke price sells nothing", {
  r <- resolve_round("bertrand_homogeneous", list(choke = 100, mc = 0), two(100, 120))
  expect_equal(r$quantity, c(0, 0))
  expect_equal(r$profit, c(0, 0))
})

test_that("bertrand: marginal cost enters profit", {
  r <- resolve_round("bertrand_homogeneous", list(choke = 100, mc = 10), two(30, 50))
  expect_equal(r$profit[r$firm == "A"], 70 * (30 - 10))
})

# ---- Hotelling --------------------------------------------------------------

test_that("hotelling: symmetric prices split 50/50", {
  r <- resolve_round("hotelling_linear", hp(), two(120, 120))
  expect_equal(r$share, c(0.5, 0.5))
  expect_equal(r$quantity, c(50, 50))
})

test_that("hotelling: interior shares sum to 1 and track the price gap", {
  r <- resolve_round("hotelling_linear", hp(), two(100, 140))
  expect_equal(sum(r$share), 1)
  expect_equal(r$share[r$firm == "A"], 0.5 + (140 - 100) / (2 * 1 * 100))  # 0.7
})

test_that("hotelling: a rival priced at/above V faces residual monopoly demand", {
  r <- resolve_round("hotelling_linear", hp(), two(150, 260))
  expect_equal(r$share[r$firm == "A"], min(1, (250 - 150) / (1 * 100)))    # 1
  expect_equal(r$share[r$firm == "B"], 0)                                  # priced out
})

test_that("hotelling: being a full capture_gap cheaper takes the whole market", {
  r <- resolve_round("hotelling_linear", hp(capture_gap = "tL"), two(100, 220))
  expect_equal(r$share[r$firm == "A"], 1)
  expect_equal(r$share[r$firm == "B"], 0)
})

test_that("hotelling: capture_gap can be overridden with a constant (bug-compat)", {
  s <- two(120, 185)                       # gap = 65
  r_tL  <- resolve_round("hotelling_linear", hp(t = 0.5, capture_gap = "tL"), s)  # gap 50
  r_100 <- resolve_round("hotelling_linear", hp(t = 0.5, capture_gap = 100), s)
  expect_equal(r_tL$share[1], 1)           # 65 >= 50  -> residual, capped
  expect_equal(r_100$share[1], 1)          # interior 1.15 -> clamped to 1
})

test_that("hotelling: clamp_share = FALSE exposes the raw (possibly <0) sheet value", {
  r <- resolve_round("hotelling_linear",
                     hp(t = 0.5, capture_gap = 100, clamp_share = FALSE),
                     two(185, 120))        # A is 65 dearer; interior with gap 100
  expect_lt(r$share[1], 0)                 # 0.5 + (120-185)/100 = -0.15
})

test_that("unknown demand model errors", {
  expect_error(resolve_round("nope", list(), two(1, 2)), "unknown demand model")
})
