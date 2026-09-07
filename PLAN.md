# Pricing Game Platform — Plan

A modular R/Shiny replacement for the Google Sheets Bertrand / Hotelling
classroom simulations.

Status: **draft for review** · Last updated: 2026-09-02

---

## 1. Context — what the current setup does

Three games, run as **fixed pairs** (2-firm markets), **repeated rounds** (see table -- but these should be parameters which are easily changed), with
**per-round profitability feedback**:

| Game                     | Rounds (2026) | Diff. param | NE price | "Monopoly" price shown |
|--------------------------|---------------|-------------|----------|------------------------|
| Bertrand (homogeneous)   | 6             | –           | ~$0 (=MC)| ~$50                   |
| Hotelling "high" (t = 1) | 10            | t = 1       | $100     | $200                   |
| Hotelling "low" (t = 0.5)| 10            | t = 0.5     | $100     | $225                   |

Workflow today: students type prices into a shared sheet; the instructor hand-picks
rivals, copy-fills formulas, advances rounds, then pools everything and builds charts
in R (`zzz-2026-output/code_thinking_about_pricing_game.R`).

### Decoded differentiated-demand formula

From the instructor's `IFS(...)` share formula. Own price `p_i`, rival `p_j`;
`O2 = V` (reservation price), `O3 = t` (transport cost per block); street length
**L = 100 "blocks"**. Returns market **share** in `[0, 1]`:

- **Interior competition:** `share_i = 0.5 + (p_j − p_i) / (2·t·100)`
  (indifferent consumer at `x* = 50 + (p_j − p_i)/(2t)`, as a fraction of the street)
- **Effective monopoly** (rival out, `p_j ≥ V`, or rival ≥ `t·L` more expensive):
  `share_i = min(1, (V − p_i)/(t·100))`
- **Zero share** if `p_i ≥ V`, or if you are ≥ `t·L` more expensive than the rival

  (The 2026 sheet hard-coded this gap at $100 for all games; the engine uses `t·L`,
  which matches the interior formula and is correct for t ≠ 1.)

Then `quantity = share · M`, `profit = quantity · (p_i − c)`.

This reduced form has **no coverage decay** — in the interior `s_i + s_j = 1` for
any prices below `V`. The rebuilt games (`config/games.yaml`) turn on
`coverage_decay: true`, which switches `hotelling_share()` to a proper linear-city
model with a reservation price: a consumer buys only if `V − p − t·d ≥ 0`, so once
both prices pass the covered boundary `V − t·L/2` the middle of the street buys
nothing and `s_i + s_j < 1`. This single expression also subsumes the
residual-monopoly and full-capture cases continuously (no `capture_gap` needed),
and it makes `collusive_price` land exactly on the textbook `V − t·L/2` — so the
NE and monopoly reference lines the slides draw are now internally consistent.
`coverage_decay: false` (the default) reproduces the 2026 sheet; the parity test
forces it off.

Bertrand is a **different demand system** in the sheet: downward-sloping
`Q(p) = 100 − p`, lowest price takes all demand, ties split 50/50 — not just the
`t → 0` limit. The engine must support both:

1. **Price-dependent total demand** (Bertrand)
2. **Fixed market size split by shares** (Hotelling)

### Problems that motivate the rebuild

- `all_profits.csv` contains `Hoteling .5, .6, .7 … .12` — a drag-fill artifact from
  hand-copying formulas.
- The R teaching sim (`simulate_market()`, `v = 250`, discrete `0:100` line, linear
  transport) uses **different math** than the spreadsheet game — the best-response
  slides don't match the engine students actually played against.
- Names are the only identifier during play; rivals chosen by hand each game.
- A live sheet lets students watch a rival's cell fill in before committing.

---

## 2. Decisions locked in

| Area        | Decision |
|-------------|----------|
| **Stack**   | R + Shiny. One app, role via URL query: `?role=player` (default), `?role=admin&key=…`, `?mode=display` (projector). |
| **Store**   | SQLite (`DBI` + `RSQLite`), polled (~1–2 s) for cross-client updates. Auto CSV snapshots. |
| **Hosting** | Runs on the instructor laptop, exposed via ngrok / Tailscale for the class hour. SQLite file on disk. No paid always-on host. |
| **v1 games**| Bertrand, Hotelling t = 1, Hotelling t = 0.5 — faithful to 2026. All 10-round repeated, fixed pairs, commit-then-reveal each round. |
| **Rounds**  | Per-game parameter (`rounds:` in `games.yaml`), not hard-coded. 2026 values: Bertrand 6, both Hotelling 10. |
| **Pairing** | App auto-generates pairs when the instructor starts a game — **rotation-aware** so rivals differ across the three games where the roster allows. Instructor can drag to override on the admin grid. Pairs **fixed for all rounds of that game**, re-generated per game. Odd headcount → one bot firm. (`random_each_round` available later as a contrast treatment.) |
| **Scale**   | One section, ~10–20 students, this term. |
| **Later**   | Advertising-budget decision, random re-pairing, random / hidden horizon, 3-firm / capacity markets, collusion-aware scoring. Each = config + one engine function, no UI rewrite. |

---

## 3. Pedagogical suggestions

1. **Commit-then-reveal each round.** All decisions lock simultaneously; nobody sees
   the rival's number until the instructor advances. Removes the "watch the cell fill
   in" artifact.
2. **Predict → observe → explain.** Before round 1, each student predicts the NE
   price, the rival's price, and their own best response. A one-sentence **"why did
   you pick this price?"** box every round. Turns the dataset into teachable-moment
   gold — real quotes against real outcomes, systematizing the "Actual $10 vs Best
   Response $49" slide.
3. **Auto-generate best-response panels.** The instructor's highest-value manual step
   (Steve round 3; Wilson vs Habib round 8) is fully automatable: for any pair/round,
   plot each firm's profit vs its own price given the rival's *actual* price; mark
   actual choice, best response, NE, monopoly.
4. **Auto-flag teachable rounds:** biggest profit swing; biggest deviation from best
   response; sustained-above-NE ("collusion"); undercut cascades.
5. **Score against benchmarks.** Report profit relative to the NE benchmark and the
   collusive benchmark so students across markets/treatments are comparable. Announce
   the scoring rule upfront.
6. **Use pairing as a lesson.** Fixed pairs for a block → tacit collusion / repeated
   dynamics. A block with random re-pairing each round kills collusion and isolates
   the one-shot NE. Run both and contrast.
7. **Deliberate treatment ladder:** Bertrand paradox → break it (capacity limit *or*
   3 firms) → differentiation restores profit → vary `t` within the *same* pairs for
   clean comparative statics → optional repeated-game block with random continuation.
   Advertising slots in as a non-price-competition / prisoner's-dilemma block.
8. **Horizon options.** A **known** 10-round horizon invites backward-induction
   unraveling in theory (students mostly still cooperate — itself a teachable gap).
   Make horizon a config option: fixed/known, hidden end round, or random
   continuation (each round ends with probability δ). Run known vs random and contrast.
9. **Anonymity toggle.** Show rivals as "Firm B" during play, real names only in
   debrief.
10. **Low-friction sign-in.** No passwords. Roster dropdown or a 4-letter join code;
    identity persisted in URL/localStorage so a refresh doesn't lose them. Handle odd
    headcounts (instructor bot / 3-firm market / bye), late joiners, dropouts
    (carry forward last price, flagged).
11. **Pre-game sandbox.** A no-stakes single-player screen to learn the interface and
    feel the demand system — without giving away the competitive discovery.
12. **Room instrumentation.** Visible round timer, "7 of 8 submitted", projector page
    with live price paths + leaderboard, optional auto-advance when all in or timer
    expires.

---

## 4. Technical design

### 4.1 Repository layout

```
pricing_game/
  app.R                      # thin: routes role -> module
  R/
    engine/demand.R          # bertrand_homogeneous(), hotelling_linear()
    engine/payoff.R          # profit = revenue - cost (- ad_spend later)
    engine/equilibria.R      # numeric ne/collusive + best_response (model-agnostic);
                             #   closed forms kept as a sanity check
    engine/resolve.R         # resolve_round(model, params, submissions) -> results
    db.R                     # schema, connect, migrations, snapshot
    pairing.R                # fixed | random_each_round | random_once; odd-N handling
    mod_player.R
    mod_admin.R
    mod_display.R
    mod_debrief.R
  config/games.yaml          # declarative game / treatment definitions
  tests/testthat/            # engine unit tests + parity vs 2026 CSVs
  debrief.qmd                # parametrized report, reuses R/engine/equilibria.R
  data/                      # sqlite file + snapshots (gitignored)
  zzz-2026-output/           # existing reference material (unchanged)
```

### 4.2 Engine contract (the modular core)

Every demand model is a pure function. Two entry points:

```r
resolve_round(model, params, submissions_df) ->   # one 2-firm market, one round
  data.frame(firm, <decisions...>, share, quantity, revenue, cost, profit, rival_price)

resolve_game(game_cfg, submissions_long) ->       # whole game; adds cumulative_profit
  data.frame(market_id, round, firm, price, rival_price, share, quantity,
             revenue, cost, profit, cumulative_profit)
```

`config/games.yaml` drives everything — rounds, pairing mode, reveal rule, which
decision inputs the UI renders, demand model + params, and benchmark prices (literal
or `compute`). Adding "ad budget" later = new `decisions:` field + `hotelling_linear_ads()`
+ yaml entry; no UI rewrite.

Example entry:

```yaml
- id: hotelling_high
  label: "Differentiated (t = 1)"
  rounds: 10
  pairing: fixed              # fixed | random_each_round | random_once
  horizon: known             # known | hidden_end | random_continuation
  reveal: per_round
  demand:
    model: hotelling_linear
    params: { V: 250, t: 1, L: 100, M: 100, mc: 0, capture_gap: tL }
  decisions: [price]
  benchmarks: { ne: compute, monopoly: compute }
```

`capture_gap: tL` uses the economically consistent full-capture threshold `t·L`
(so t = 0.5 → $50, not the sheet's hard-coded $100). A `capture_gap: 100` value
reproduces the 2026 sheet exactly if needed for the parity check.

### 4.3 Database schema

- `sessions(id, game_id, current_round, status, config_json, created_at)`
- `players(id, session_id, display_name, firm_label, market_id, token, joined_at)`
- `submissions(session_id, round, player_id, decisions_json, rationale, prediction_json, submitted_at)`
- `results(session_id, round, player_id, quantity, revenue, profit, cumulative_profit, share, computed_at)`

### 4.4 Pages

- **Player:** join by name / code → lobby ("waiting to be paired") → round view (your
  firm, rival as "Firm B" by default, round N/10, timer, price input, "why this
  price?" box, optional round-1 predictions, edit until locked) → **result card**
  after each round: your price, rival price, quantity, profit, cumulative, plus a
  mini "what-if" profit curve given the rival's actual price.
- **Admin:** create session from a template · market × round grid of who has
  submitted (+ hidden / revealed prices) · buttons: open round, close, compute &
  advance, reveal to players, re-pair, insert bot (odd N), export CSV, restart.
- **Display (projector):** submission progress, live price paths vs NE / monopoly
  lines, leaderboard (raw profit and profit-vs-NE-benchmark).
- **Debrief:** in-app tab + `debrief.qmd`. Reuses the existing
  `code_thinking_about_pricing_game.R` charts. Auto-flags teachable rounds;
  predicted-vs-actual; auto-generated best-response panels for any pair/round.

---

## 5. Phasing

Each phase is runnable.

| Phase | Deliverable |
|-------|-------------|
| **0 — Spec & engine** ✅ *done* | `R/engine/` + `config/games.yaml` + 55 tests (`scripts/run_tests.R`). **Parity: exact** — all 24 player/game totals reproduce `all_profits.csv` to ≤ $0.50 (whole-dollar rounding) once `$O$3 = 2`. Finding: the 2026 "high" and "low" differentiation games used *identical* parameters (`$O$3 = 2` in both); true NE was $200, not the $100 on the slides. Full write-up + the v1 decision it raises: `docs/phase0-parity-findings.md`. |
| **1 — MVP** ✅ *done* | `app.R` + `R/db.R` `R/session_logic.R` `R/pairing.R` `R/mod_{player,admin}.R`. SQLite store (WAL, 1 s poll — live-updates on submit *and* on edit); student join/resume + submit + rationale, screen debounced so a rival's activity can't wipe an in-progress entry; admin create/start/close-score/advance/reset + CSV export; **History tab: full CRUD over saved sessions** (open/rename/delete/export, delete is the only data loss); random pairing, odd-N sits out; **no-show = scored out of the market** ($0; rival gets sole-seller outcome), carry-forward optional; per-round result cards; leaderboard. All 3 games run. `scripts/run_app.R` + tunnel. 173 tests. Docs: `docs/phase1-mvp.md`. |
| **2 — Full v1** | Both Hotelling games; display / projector page; round timer; anonymity toggle; odd-N bot; carry-forward for no-shows. |
| **3 — Debrief** | Quarto report + in-app tab; auto-flagged teachable moments; predicted-vs-actual; best-response panels. |
| **4 — Identity, roster & teams** *(deferred — see §7)* | Class layer + persistent identity so students don't re-join between games; roster with a gradebook key so exports are pre-matched; teams of 2–3 students per firm, self-named. |
| **5 — Extensions (as needed)** | Advertising decision; random re-pairing; random / hidden horizon; 3-firm / capacity markets; collusion-aware scoring. |

---

## 7. Deferred — identity, roster & teams

Not for the first classroom run. Captured so it isn't lost.

**Problems it solves**
- "New session" makes every student re-type their name — inefficient across the
  three games in one class period.
- No stable link between a student in the app and their row in the gradebook;
  matching is manual and typo-prone (the 2026 data needed a hand-built
  `name → ID` map).
- A "firm" is usually a **team of 2–3 students**, not one person — the current
  model has no notion of that.

**Intended shape**
- A **class** owns a **roster** (set up once) and holds many games. Running the
  three games becomes **"Next game"** — same participants, fresh pairing, *no
  rejoin*. "New class" is the rare action.
- **Roster** = individuals with a **gradebook key** (student ID or email). Every
  admin view and every CSV export carries that key next to the display name, so a
  gradebook merge is a plain join.
- **Teams**: students group into a firm (2–3 people) and **name the firm
  themselves**; the team is the "player" in a session. Roster keys for all
  members ride along on the team so individual credit is still recoverable.
- **Persistent identity**: token in the browser (localStorage) + URL keyed to a
  person/team, not a session; a returning student gets one-click rejoin, or
  re-picks from a dropdown if the browser was cleared.
- `players.team_id` / `teams.roster_ids[]`; `sessions.class_id`.

Open sub-questions for when we pick this up: roster source (paste vs CSV vs LMS),
impersonation check (name dropdown vs PIN), how teams form (self-select in a
lobby vs instructor-assigned), whether class state spans multiple meetings.

## 6. Parameters — confirmed

Spreadsheet cells: `$O$2 = V = 250`, `$O$3 = t = 2` **in both differentiated tabs**.
`$O$3` is the differentiation / transport parameter; `L = 100` is the "/100" in the
sheet formula.

| Parameter | Bertrand | Hotelling ("Hoteling 1") | Hotelling ("Hoteling .5") |
|---|---|---|---|
| Demand | `D = 100 − P`, low price serves all, ties split 50/50 | share formula (§1) | share formula (§1) |
| `V` (reservation, `$O$2`) | 100 (choke) | 250 | 250 |
| `t` (`$O$3`) | – | **2** (as played) | **2** (as played) |
| `M` (market size) | n/a | 100 | 100 |
| `c` (marginal cost) | 0 | 0 | 0 |
| `L` (street) | – | 100 | 100 |
| Full-capture gap | – | literal **100** (not `t·L`) | literal **100** |
| Rounds (2026) | 6 | 10 | 10 |

**The 2026 "high" and "low" games were identical** — `$O$3` was 2 in both.
Parity is exact (≤ $0.50 / player). See `docs/phase0-parity-findings.md`.

### Benchmarks (`benchmarks: {ne: compute, ...}`)

`resolve_benchmarks()` fills any `compute`/missing entry. `ne` and `collusive` are
solved **numerically** against the actual demand model (`ne` = fixed point of the
best response; `collusive` = argmax of joint profit) so they auto-follow any model
or param change; `monopoly` is the closed-form textbook reference. All three games
resolve in ~60 ms total, done once at session start. Closed forms
(`ne_price()` etc.) are kept and unit-tested against the numeric ones.

| | Bertrand | Hotelling t = 1 | Hotelling t = 0.5 |
|---|---|---|---|
| `ne` (= c + t·L) | $0 | $100 | $50 |
| `monopoly` (textbook `V − t·L/2`, slide line) | $50 | $200 | $225 |
| `collusive` (numeric joint-profit optimum) | $50 | $200 | $225 |

With `coverage_decay: true` (the rebuilt games) `collusive` meets `monopoly` — the
market empties from the middle past `V − t·L/2`, so joint profit peaks there. In
the raw 2026 reduced form (no coverage decay) `collusive` would instead be `~$250`
(share stays 0.5 up to `V`); the parity test runs in that mode.

### v1 design — settled

The rebuilt Hotelling games use the *intended* design (`t = 1` / `t = 0.5`) —
genuine high vs low differentiation, and the slide reference lines
(`NE $100 / $50`, `monopoly $200 / $225`) are correct. `config/games.yaml`
encodes this; `R/parity.R` overrides to `t = 2` only to regression-test against
the 2026 numbers.
