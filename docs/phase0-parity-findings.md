# Phase 0 — engine, config, and parity findings

Built:

- `R/engine/` — pure functions, no package deps beyond `yaml`:
  - `demand.R` — `demand_bertrand_homogeneous()`, `demand_hotelling_linear()`
  - `payoff.R` — `apply_costs()` (advertising-spend hook, inert in v1)
  - `resolve.R` — `resolve_round()` (one 2-firm market) and `resolve_game()`
    (a whole game from a long submissions table, adds `cumulative_profit`)
  - `equilibria.R` — **numeric** `ne_price_numeric()` (best-response fixed point)
    and `collusive_price_numeric()` (argmax joint profit), plus a vectorised
    regime-aware `best_response()`; all model-agnostic via `own_profit()`.
    Closed forms (`ne_price()`, `monopoly_price()` = textbook `V − t·L/2`,
    `collusive_price()`) kept and unit-tested against the numeric ones.
    `resolve_benchmarks()` uses numeric by default (~60 ms for all 3 games).
  - `load_games.R` — `load_games()`, `get_game()`, `resolve_benchmarks()`
- `config/games.yaml` — `bertrand`, `hotelling_high`, `hotelling_low`
- `R/parity.R` + `scripts/run_parity.R` — re-run 2026 prices, diff vs `all_profits.csv`
- `tests/testthat/` — 55 assertions (`Rscript scripts/run_tests.R`)

## Parity result — RESOLVED, exact

`Rscript scripts/run_parity.R`, with the 2026-sheet parameters
(`V = 250`, **`t` (`$O$3`) = 2**, `L = 100`, `M = 100`, `mc = 0`,
`capture_gap = 100`):

| Game            | max abs diff | verdict |
|-----------------|-------------:|---------|
| `bertrand`      | **$0.29**    | reproduces to display rounding ✓ |
| `hotelling_high`| **$0.36**    | reproduces to display rounding ✓ |
| `hotelling_low` | **$0.50**    | reproduces to display rounding ✓ |

Every one of the 24 player/game totals matches `all_profits.csv` to within
whole-dollar rounding. The engine is a faithful reimplementation of the 2026
sheet.

### The key finding

The instructor supplied `$O$2 = 250` and `$O$3 = 2`. Plugging `$O$3 = 2` into
the share formula reproduces **both** differentiated games exactly — including
`hotelling_low` ("Hoteling .5"). So:

- **The 2026 "high" and "low" differentiation games used identical parameters.**
  `$O$3` was 2 in both tabs. The `t = 1` / `t = 0.5` distinction — and the
  matching `NE = $100`, `monopoly = $200 / $225` reference lines on the slides —
  was the instructor's *intended* framing, not what the sheet computed.
- **The game's actual NE was `mc + t·L = $200`**, not $100. The `50 + p_j/2`
  best-response line on the 2026 slides corresponds to `$O$3 = 1`; the sheet ran
  `$O$3 = 2`, whose best response is `100 + p_j/2` and whose symmetric NE is $200.
  This may be *why* several pairs converged on ~$200 — it was closer to the true
  equilibrium of the game as parameterised than the $100 they were told.
- **The 2026 reduced-form demand has no coverage decay.** At any symmetric price
  below `V`, each firm's share is exactly 0.5, so joint profit rises with price all
  the way to `V = 250` — the "$200 / $225 monopoly" figures are the coverage-
  constrained textbook value `V − t·L/2`, not what that sheet rewarded.
  **The rebuilt games (`config/games.yaml`) turn on `coverage_decay: true`**: a
  proper linear-city model with a reservation price, where the middle of the
  street stops buying once both prices pass `V − t·L/2`. That makes the collusive
  price meet the textbook line, so NE / monopoly / collusive are all consistent
  with the slides. `coverage_decay: false` (default) still reproduces 2026, and
  the parity test forces it off.
- **`capture_gap` must be the literal `100`.** With `t = 2` the economically
  consistent gap would be `t·L = 200`; using it moves the high game off by 2.6%
  RMS. The sheet's hard-coded `100` is load-bearing for exact reproduction and is
  set explicitly in the parity overrides.

### As-designed vs as-played

`run_parity(as_designed = TRUE)` runs the same 2026 prices through
`config/games.yaml` as written (`t = 1` / `t = 0.5`). It diverges from
`all_profits.csv` by up to 72% — i.e. the games the slides describe are **not**
the games that were played. `config/games.yaml` carries the intended design;
`R/parity.R` overrides to `t = 2` to reproduce history.

## v1 decision — settled

The two rebuilt Hotelling games **will genuinely differ** (`t = 1` vs `t = 0.5`),
so the slide reference lines (`NE = $100` / `$50`, `monopoly = $200` / `$225`)
become correct and the "more differentiation → more market power" lesson is real.
`config/games.yaml` already encodes this. `R/parity.R` keeps the `t = 2` override
purely to reproduce the 2026 numbers for regression testing.

Phase 0 is done and green.
