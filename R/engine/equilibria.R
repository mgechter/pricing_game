# Benchmarks — Nash, collusive, and best response.
#
# Two layers:
#   * closed forms  (`ne_price`, `monopoly_price`, `collusive_price`, ...) — fast,
#     hand-derived, model-specific. Used as a sanity check in the tests.
#   * numerical     (`ne_price_numeric`, `collusive_price_numeric`, `best_response`)
#     — model-agnostic: they optimise the ACTUAL demand model via `own_profit()`,
#     so they keep working when a model or its params change (e.g. an advertising
#     term later). `resolve_benchmarks()` uses these by default.
#
# All of this reads the same params the live game uses, so a reference line on a
# slide can no longer silently disagree with the engine.

# ============================ closed forms =================================

# Nash price (symmetric, one-shot).
#   Hotelling covered NE is  mc + t*L.  With coverage_decay, if that price would
#   fall in the partial-coverage region the equilibrium sits at the covered
#   boundary V - t*L/2 instead (exact while the partial-region optimum
#   (V+mc)/2 <= that boundary; otherwise use ne_price_numeric()).
ne_price <- function(model, params) {
  switch(model,
    bertrand_homogeneous = params$mc %||% 0,                                    # p = mc
    hotelling_linear = {
      mc <- params$mc %||% 0
      L  <- params$L %||% 100
      ne_covered <- mc + params$t * L
      if (isTRUE(params$coverage_decay))
        min(ne_covered, params$V - params$t * L / 2)
      else
        ne_covered
    },
    stop("no ne_price() for model: ", model, call. = FALSE)
  )
}

# "Monopoly" reference the 2026 slides draw:
#   Bertrand  : single-firm monopoly on Q = choke - p   -> (choke + mc)/2
#   Hotelling : coverage-constrained textbook value      -> V - t*L/2
monopoly_price <- function(model, params) {
  switch(model,
    bertrand_homogeneous = {
      choke <- params$choke %||% params$V %||% 100
      (choke + (params$mc %||% 0)) / 2
    },
    hotelling_linear = params$V - params$t * (params$L %||% 100) / 2,
    stop("no monopoly_price() for model: ", model, call. = FALSE)
  )
}

# Residual-demand monopoly for a Hotelling firm whose rival has exited.
monopoly_price_residual <- function(model, params) {
  if (model != "hotelling_linear")
    stop("monopoly_price_residual() is Hotelling-only", call. = FALSE)
  (params$V + (params$mc %||% 0)) / 2
}

# Collusive price = the symmetric price that maximises joint profit.
#   coverage_decay = TRUE : the market empties from the middle once p passes the
#     covered boundary, so joint profit peaks there -> V - t*L/2  (= monopoly_price;
#     holds while mc + t*L <= V, else use collusive_price_numeric()).
#   coverage_decay = FALSE: no coverage decay, so joint profit climbs to the
#     reservation price -> V. Higher than the textbook line by t*L/2.
collusive_price <- function(model, params) {
  if (model != "hotelling_linear") return(monopoly_price(model, params))
  if (isTRUE(params$coverage_decay)) return(monopoly_price(model, params))
  params$V
}

# Closed-form interior best response for hotelling_linear (no regime checks):
#   argmax_p  M (p - mc) [0.5 + (pj - p)/(2 t L)]   ->   p* = (mc + t L + pj) / 2
best_response_interior_hotelling <- function(params, rival_price) {
  ((params$mc %||% 0) + params$t * (params$L %||% 100) + rival_price) / 2
}

# ============================ numerical ====================================

# Price range to search for a model (marginal cost .. choke / reservation price).
.benchmark_range <- function(model, params) {
  hi <- switch(model,
    bertrand_homogeneous = params$choke %||% params$V %||% 100,
    hotelling_linear     = params$V,
    1000)
  c(lo = params$mc %||% 0, hi = hi)
}

# Best response to a fixed rival price: profit-maximising own price plus the full
# profit curve (for the debrief's best-response panels). Regime-aware — it
# evaluates the real demand model. Vectorised; a coarse grid then a local refine.
best_response <- function(model, params, rival_price, grid = NULL, by = NULL,
                          n_coarse = 600, n_refine = 400) {
  rng <- .benchmark_range(model, params)
  if (is.null(grid)) {
    grid <- if (is.null(by)) seq(rng[["lo"]], rng[["hi"]], length.out = n_coarse)
            else              seq(rng[["lo"]], rng[["hi"]], by = by)
  }
  profit <- own_profit(model, params, grid, rival_price)
  best   <- grid[which.max(profit)]
  bprof  <- max(profit)

  if (identical(model, "bertrand_homogeneous")) {
    # Bertrand profit jumps at the rival's price, so a grid lands a hair below
    # it and a label rounds that to the rival's own price, where a tie earns
    # half. Prices are set in whole cents (`tick`), so the best reply is one of:
    # marginal cost, a cent under the rival, a tie at the rival's price, or the
    # monopoly price. The grid curve is kept for plotting.
    tick <- params$tick %||% 0.01
    mc   <- params$mc %||% 0
    cand <- c(mc, round(rival_price - tick, 8), rival_price, monopoly_price(model, params))
    cand <- unique(cand[is.finite(cand) & cand >= mc])
    pr   <- own_profit(model, params, cand, rival_price)
    i    <- which.max(pr)
    return(list(price = cand[i], profit = pr[i],
                curve = data.frame(price = grid, profit = profit)))
  }

  if (is.null(by) && length(grid) > 2) {                 # local refine
    step <- grid[2] - grid[1]
    fine <- seq(max(rng[["lo"]], best - step),
                min(rng[["hi"]], best + step), length.out = n_refine)
    fp   <- own_profit(model, params, fine, rival_price)
    best <- fine[which.max(fp)]
    bprof <- max(fp)
  }
  list(price = best, profit = bprof,
       curve = data.frame(price = grid, profit = profit))
}

# Numerical Nash price: the symmetric fixed point of the best response.
# `g(p) = best_response(p) - p` is positive below the NE and negative above it;
# we bracket the sign change on a coarse grid and bisect. If `g` never turns
# positive (Bertrand: undercutting always beats matching) the equilibrium is the
# corner at marginal cost.
ne_price_numeric <- function(model, params, tol = 1e-3, n_scan = 25) {
  rng <- .benchmark_range(model, params)
  g   <- function(p) best_response(model, params, p)$price - p

  scan <- seq(rng[["lo"]], rng[["hi"]], length.out = n_scan)
  gv   <- vapply(scan, g, numeric(1))
  k    <- which(head(gv, -1) > 0 & tail(gv, -1) <= 0)[1]
  if (is.na(k)) return(unname(rng[["lo"]]))               # corner equilibrium

  a <- scan[k]; b <- scan[k + 1]
  for (i in seq_len(80)) {
    m  <- (a + b) / 2
    gm <- g(m)
    if (abs(gm) < tol || (b - a) < tol) return(m)
    if (gm > 0) a <- m else b <- m
  }
  (a + b) / 2
}

# Numerical collusive price: the symmetric price that maximises joint profit.
collusive_price_numeric <- function(model, params, n_coarse = 800, n_refine = 400) {
  rng  <- .benchmark_range(model, params)
  grid <- seq(rng[["lo"]], rng[["hi"]], length.out = n_coarse)
  jp   <- 2 * own_profit(model, params, grid, grid)       # symmetric: both at `grid`
  b    <- grid[which.max(jp)]
  step <- grid[2] - grid[1]
  fine <- seq(max(rng[["lo"]], b - step), min(rng[["hi"]], b + step),
              length.out = n_refine)
  fjp  <- 2 * own_profit(model, params, fine, fine)
  fine[which.max(fjp)]
}
