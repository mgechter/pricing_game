# Phase 1 — playable MVP

A working Shiny app: students join, submit a price each round, and see their
result; the instructor runs the round loop and exports the data. All three games
work (Bertrand + both Hotelling); the round loop, pairing, and scoring are the
same code the parity test validates.

## Run it

```sh
Rscript scripts/run_app.R                     # http://127.0.0.1:3838
PRICING_ADMIN_KEY=secret Rscript scripts/run_app.R
```

Expose it to the class for the hour with a tunnel:

```sh
ngrok http 3838
# or:  tailscale funnel 3838
```

| view | URL |
|---|---|
| Student | `http://<host>:3838/` |
| Instructor | `http://<host>:3838/?role=admin&key=<PRICING_ADMIN_KEY>` (default key `admin`) |

Env vars: `PRICING_DB` (sqlite path, default `data/pricing_game.sqlite`),
`PRICING_ADMIN_KEY`, `PRICING_GAMES`, `PRICING_PORT`.

## Flow

1. **Instructor** opens the admin URL → picks a game → *Create session*.
2. Shares the student link. Each **student** enters a name → lands in the lobby.
   (Their identity is kept in the URL as `?t=…`, so a refresh resumes them.)
3. Instructor clicks **Start game** — players are randomly paired into 2-firm
   markets, fixed for the game. Odd headcount → the last player sits out (a bot /
   instructor firm comes in Phase 2).
4. Each round: students enter a price (and an optional "why this price?"), can
   change it until the round closes. Rival is shown only as "the other firm".
5. Instructor watches the market grid fill in — it updates live as students
   submit *and* as they change an answer — then **Close & score round**.
6. Students see their result card — price, rival price, units, round profit,
   cumulative. Instructor clicks **Next round**.
7. After the last round: leaderboard, and **Export CSV** (one row per player per
   round: prices, shares, quantities, profit, cumulative, rationale, carried flag).

**Missing a price:** a no-show is scored *out of the market* — $0 that round,
while their rival gets the sole-seller outcome at its own price. A diligent
player is never held back by a slow rival. Tick **carry forward** before scoring
to instead reuse that player's previous-round price; re-score anytime.

**History tab (admin):** every session is saved automatically. The History table
lists them all — Open (make active again), export CSV, Rename, or Delete a false
start. Deleting is the only thing that removes data.

*Reset to lobby* replays the same roster (clears that session's results);
*New session* leaves the old one in History and starts fresh.

## Persistence

Everything is in one SQLite file (`data/…sqlite`, git-ignored, WAL mode). Stopping
and restarting the app resumes the session exactly. The app polls the DB every
second, so every browser (students + admin + future projector) stays in sync
without a refresh.

## What's not here yet (Phase 2+)

Projector/display page · round timer · bot / instructor firm for odd headcount ·
anonymity toggle wording · rotation-aware pairing across games · the debrief.

## Tests

`Rscript scripts/run_tests.R` — 150 assertions. `test-session-logic.R` drives the
whole lobby→finished loop headless (pairing, tie-splitting, carry-forward, guard
rails, JSON round-trip); `test-ui-screens.R` renders every admin/player screen
against real game state.
