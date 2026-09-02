# Small shared helpers for the engine. Base R only — no package dependencies.

# NULL / empty coalescing: `a %||% b` returns `b` when `a` is NULL or length 0.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Clamp a numeric vector into [lo, hi].
.clamp <- function(x, lo = 0, hi = 1) pmin(pmax(x, lo), hi)

# Resolve the full-capture price gap for a Hotelling game.
# "tL" (the default) means the economically consistent threshold t * L.
# A number reproduces the 2026 sheet's hard-coded value (e.g. 100).
.capture_gap <- function(params) {
  g <- params$capture_gap
  if (is.null(g) || identical(g, "tL")) return(params$t * (params$L %||% 100))
  as.numeric(g)
}

.require_two_firms <- function(submissions) {
  if (nrow(submissions) != 2L) {
    stop("v1 demand models expect exactly 2 firms per market; got ",
         nrow(submissions), call. = FALSE)
  }
}
