# Payoff assembly.
#
# In v1 the only decision is `price` and profit = revenue - cost is computed inside
# the demand models. This hook is where non-price decisions (e.g. an advertising
# budget) subtract additional costs, so the demand models stay focused on demand.
# It is wired in but inert until a game declares such a decision.

apply_costs <- function(results, submissions = NULL, params = list()) {
  res <- results
  if (!is.null(submissions) && "ad_budget" %in% names(submissions)) {
    ad      <- submissions$ad_budget[match(res$firm, submissions$firm)]
    ad_cost <- (params$ad_cost %||% 1) * ad
    res$cost   <- res$cost + ad_cost
    res$profit <- res$profit - ad_cost
  }
  res
}
