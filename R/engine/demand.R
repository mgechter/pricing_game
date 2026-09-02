# Demand models.
#
# Each model exposes:
#   * a vectorised primitive — `<model>_quantity()` / `hotelling_share()` — giving
#     the "own" firm's demand as a function of its own price (vector) against a
#     fixed rival price. Used by resolve_round() AND by the numerical benchmark
#     solvers in equilibria.R, so there is one source of truth.
#   * `demand_<model>(submissions, params)` — the round resolver, one row per firm:
#     firm, price, share, quantity, revenue, cost, profit, rival_price
#
# `submissions` : data.frame with `firm` and `price` (exactly 2 rows in v1).
# `params`      : named list, taken verbatim from config/games.yaml.
#
# Adding a market structure = add a primitive + a `demand_*` here and register
# both in resolve.R.

# ---------------------------------------------------------------------------
# Bertrand — homogeneous good
# ---------------------------------------------------------------------------
# Market demand at the winning (lowest) price:  Q(p) = max(0, choke - p).
# The lower price serves the whole market; equal prices split it; a price at or
# above `choke` sells nothing.

bertrand_quantity <- function(p_own, p_rival, params) {
  choke <- params$choke %||% params$V %||% 100
  q_at  <- pmax(0, choke - p_own)              # demand if this firm serves
  ifelse(p_own >= choke, 0,
    ifelse(p_own < p_rival, q_at,
      ifelse(p_own == p_rival, q_at / 2, 0)))
}

demand_bertrand_homogeneous <- function(submissions, params) {
  .require_two_firms(submissions)
  mc    <- params$mc %||% 0
  choke <- params$choke %||% params$V %||% 100

  p    <- submissions$price
  q    <- bertrand_quantity(p, rev(p), params)
  Qtot <- max(0, choke - min(p))
  share <- if (Qtot > 0) q / Qtot else rep(0, length(p))

  data.frame(
    firm        = submissions$firm,
    price       = p,
    share       = share,
    quantity    = q,
    revenue     = p * q,
    cost        = mc * q,
    profit      = q * (p - mc),
    rival_price = rev(p),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Hotelling — linear city, differentiated products
# ---------------------------------------------------------------------------
# Two variants, selected by `coverage_decay` in params:
#
# coverage_decay = FALSE (default) — the 2026 spreadsheet share formula. For own
#   price pi, rival pj, reservation V, differentiation t ($O$3), street L, gap g:
#     pi >= V                    -> 0
#     pi - pj >= g               -> 0                          (priced out)
#     pj >= V OR pj - pi >= g    -> min(1, (V - pi)/(t*L))     (residual monopoly)
#     otherwise                  -> 0.5 + (pj - pi)/(2*t*L)    (interior)
#   `clamp_share` (default TRUE) guards the interior branch if g is a stale const.
#   Full coverage in the interior: s_own + s_rival = 1 for any prices below V.
#
# coverage_decay = TRUE — proper linear-city Hotelling with a reservation price.
#   A consumer at distance d from a firm buys only if V - p - t*d >= 0, so when
#   both prices are high the middle of the street buys nothing and
#   s_own + s_rival < 1. Firm serves [0, served] where
#     served = clamp( min( x*, (V - p_own)/t ), 0, L ),
#     x* = L/2 + (p_rival - p_own)/(2t)           (indifferent consumer)
#   This one expression also covers the residual-monopoly and full-capture cases
#   continuously, so `capture_gap` is not used in this mode.

hotelling_share <- function(p_own, p_rival, params) {
  V <- params$V
  t <- params$t
  L <- params$L %||% 100

  if (isTRUE(params$coverage_decay)) {
    xstar  <- L / 2 + (p_rival - p_own) / (2 * t)
    reach  <- (V - p_own) / t
    served <- pmin(pmax(pmin(xstar, reach), 0), L)
    return(.clamp(served / L, 0, 1))
  }

  g <- .capture_gap(params)
  interior <- 0.5 + (p_rival - p_own) / (2 * t * L)
  residual <- pmin(1, (V - p_own) / (t * L))
  s <- ifelse(p_own >= V, 0,
        ifelse(p_own - p_rival >= g, 0,
          ifelse(p_rival >= V | p_rival - p_own >= g, residual, interior)))
  if (isTRUE(params$clamp_share %||% TRUE)) s <- .clamp(s, 0, 1)
  s
}

demand_hotelling_linear <- function(submissions, params) {
  .require_two_firms(submissions)
  M  <- params$M %||% 100
  mc <- params$mc %||% 0

  p_own   <- submissions$price
  p_rival <- rev(p_own)
  share   <- hotelling_share(p_own, p_rival, params)
  q       <- share * M

  data.frame(
    firm        = submissions$firm,
    price       = p_own,
    share       = share,
    quantity    = q,
    revenue     = p_own * q,
    cost        = mc * q,
    profit      = q * (p_own - mc),
    rival_price = p_rival,
    stringsAsFactors = FALSE
  )
}
