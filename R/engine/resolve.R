# Round / game resolution — the single entry point the app and the debrief call.

# Registry of demand models. Add a row to extend the platform.
DEMAND_MODELS <- list(
  bertrand_homogeneous = demand_bertrand_homogeneous,
  hotelling_linear     = demand_hotelling_linear
)

# Vectorised "own firm quantity" primitives, keyed the same way. Used by the
# numerical benchmark / best-response solvers in equilibria.R.
OWN_QUANTITY <- list(
  bertrand_homogeneous = bertrand_quantity,
  hotelling_linear     = function(p_own, p_rival, params)
    hotelling_share(p_own, p_rival, params) * (params$M %||% 100)
)

# Profit of the "own" firm at price(s) p_own against a fixed rival price, ignoring
# any non-price decisions (v1 has none). Vectorised over p_own.
own_profit <- function(model, params, p_own, p_rival) {
  f <- OWN_QUANTITY[[model]]
  if (is.null(f)) stop("no own_profit primitive for model: ", model, call. = FALSE)
  q <- f(p_own, p_rival, params)
  q * (p_own - (params$mc %||% 0))
}

# Resolve one round of one 2-firm market.
#   model       : key into DEMAND_MODELS
#   params      : named list from games.yaml (game$demand$params)
#   submissions : data.frame with `firm`, `price` (+ future decision columns)
# Returns one row per firm: firm, price, share, quantity, revenue, cost, profit,
# rival_price. Cumulative profit is added by resolve_game(), not here.
resolve_round <- function(model, params, submissions) {
  fn <- DEMAND_MODELS[[model]]
  if (is.null(fn)) stop("unknown demand model: ", model, call. = FALSE)
  if (!all(c("firm", "price") %in% names(submissions))) {
    stop("submissions must have columns `firm` and `price`", call. = FALSE)
  }
  res <- fn(submissions, params)
  apply_costs(res, submissions, params)
}

# Resolve a whole game from a long table of submissions.
#   game            : one game config list (see load_games())
#   submissions_long: data.frame with `firm`, `market_id`, `round`, `price`
# Returns a long results table with cumulative_profit within (market_id, firm).
resolve_game <- function(game, submissions_long) {
  req <- c("firm", "market_id", "round", "price")
  if (!all(req %in% names(submissions_long))) {
    stop("submissions_long needs columns: ", paste(req, collapse = ", "), call. = FALSE)
  }
  model  <- game$demand$model
  params <- game$demand$params

  keys <- unique(submissions_long[c("market_id", "round")])
  keys <- keys[order(keys$market_id, keys$round), , drop = FALSE]

  parts <- lapply(seq_len(nrow(keys)), function(i) {
    mk <- keys$market_id[i]
    rd <- keys$round[i]
    sub <- submissions_long[submissions_long$market_id == mk &
                              submissions_long$round == rd, , drop = FALSE]
    if (nrow(sub) != 2L) {
      warning(sprintf("market %s round %s has %d submissions; skipping",
                      mk, rd, nrow(sub)), call. = FALSE)
      return(NULL)
    }
    res <- resolve_round(model, params, sub[c("firm", "price")])
    res$market_id <- mk
    res$round     <- rd
    res
  })

  res <- do.call(rbind, parts)
  res <- res[order(res$market_id, res$firm, res$round), , drop = FALSE]
  res$cumulative_profit <- stats::ave(res$profit, res$market_id, res$firm,
                                      FUN = cumsum)
  rownames(res) <- NULL
  res[c("market_id", "round", "firm", "price", "rival_price", "share",
        "quantity", "revenue", "cost", "profit", "cumulative_profit")]
}
