# Phase 1.5 — output / debrief

Everything for turning a finished game into class material: a clean data bundle,
an on-platform Debrief tab, and a Quarto report that emits a figure folder.

The Shiny tab and `debrief.qmd` both call `R/debrief_data.R` + `R/debrief_plots.R`,
so the numbers and charts can't disagree between screen and report.

## Data bundle

`sl_export_bundle(con, session_ids)` → four tidy tables:

| file | one row per | key columns |
|---|---|---|
| `results_long.csv` | player · round | price, rival_price, share, quantity, revenue, profit, cumulative_profit, no_submission |
| `submissions.csv` | player · round | price, rationale, carried (1 = carried forward, 2 = scored as no-show) |
| `pairs.csv` | player | market_id, firm_label |
| `benchmarks.csv` | session | model, rounds, V, t, L, M, mc, choke, coverage_decay, **ne / collusive / monopoly**, created_at |

Every row carries `session_id`, `game_id`, `game_label`, so a whole class period
stacks into one bundle. Written as a **ZIP of CSVs** (`write_bundle_zip`) and as a
**multi-tab `.xlsx`** (`write_bundle_xlsx`).

In the app: **History → "Download a class bundle"** — tick the sessions, then
*Download bundle (ZIP)* / *(Excel)*.

## Debrief tab (admin)

Pick a scored session:

- **Teachable moments** — auto-flagged rounds: biggest profit swing, biggest
  deviation from best response, tacit collusion (both firms > NE for ≥ 3 rounds
  running), undercut cascade (both cut price ≥ 3 rounds running). "Jump to a flag"
  drives the best-response explorer.
- **Price paths** — faceted by market, NE / collusive / monopoly reference lines,
  name ↔ "Firm A/B" toggle.
- **Average price per round** vs the same lines.
- **Best-response explorer** — market × round × firm → that firm's payoff as a
  function of its own price given the rival's actual price; marks actual choice,
  best response, NE.
- **Cumulative profit** paths.
- **Leaderboards** — players and markets, with a *vs. all-NE* column (profit minus
  what both firms would have made pricing at NE every round).

Every chart has a **PNG** button; **Figure pack (ZIP)** exports the standard set.

## Report

`debrief.qmd` — params: `db` + `session_id` (live SQLite, default = latest scored),
or `bundle_dir` (an unzipped bundle). Renders to self-contained HTML and writes
every chart to `figs/` for dropping into a slide deck. Run it from the project
root: `quarto render debrief.qmd -P session_id:5`.

`scripts/render_figs.R [session_id] [out_dir]` — the figure pack without Quarto.

## Notes

- `resolve_benchmarks()` now snaps a numeric solve onto the closed form when they
  agree within $0.50, so reference lines read `100`, not `100.001`.
- Player tokens retry on collision (`sl_add_player`) — needed once sessions share
  a token namespace.
- `figs/` is git-ignored (generated).
