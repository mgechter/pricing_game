# Pricing game

A Shiny app for running **Bertrand** and **Hotelling** pricing games in class.
Students submit a price each round on their phones/laptops; the instructor runs
the round loop from an admin screen; afterwards a **Debrief** tab and a data
export turn the results into charts and slide material.

Self-contained: one R process, one SQLite file. Runs on a laptop and is exposed
to the class for the hour with a tunnel (ngrok / Tailscale). No accounts, no
database server.

- Design & status: [`PLAN.md`](PLAN.md)
- How a class session works, in detail: [`docs/phase1-mvp.md`](docs/phase1-mvp.md)
- The output/debrief pipeline: [`docs/phase1.5-output.md`](docs/phase1.5-output.md)

---

## 1. Requirements

- **R ≥ 4.2** (developed on 4.4).
- A C toolchain for `RSQLite` / `ragg` (Xcode CLT on macOS; `build-essential` +
  `libfreetype6-dev libharfbuzz-dev libfribidi-dev` on Debian/Ubuntu).
- **Quarto** *(optional)* — only for rendering the debrief report. Bundled with
  RStudio, or from <https://quarto.org>.

Install the R packages:

```r
install.packages(c(
  "shiny", "bslib", "DBI", "RSQLite", "yaml", "jsonlite",
  "ggplot2", "dplyr", "tidyr", "scales", "writexl", "zip", "ragg",
  "testthat", "withr"        # last two: for the test suite only
))
```

---

## 2. Run it

```sh
git clone <this repo> pricing_game
cd pricing_game
Rscript scripts/run_app.R                       # serves on :3838
```

| view | URL |
|---|---|
| **Student** | `http://<host>:3838/` |
| **Instructor** | `http://<host>:3838/?role=admin&key=admin` |
| **Projector** | `http://<host>:3838/?mode=display` — read-only big screen for the room |

Expose it to the class for the session:

```sh
ngrok http 3838          # or:  tailscale funnel 3838
```

Give the class the student URL; keep the admin URL to yourself. Change the admin
key with `PRICING_ADMIN_KEY=…` (see [Configuration](#6-configuration)).

Stop the server with **Ctrl-C** in that terminal (twice if the first is
ignored). If it ever gets orphaned: `kill $(lsof -ti tcp:3838)`.

---

## 3. Running a class session

1. **Instructor** opens the admin URL → picks a game → **Create session**.
2. Shares the student link. Each **student** enters a name → lands in the lobby.
   Their identity is kept in the URL (`?t=…`), so a refresh resumes them.
3. Instructor clicks **Start game** — students are randomly paired into 2-firm
   markets, fixed for the game. Odd headcount → the last student joins a market as
   a **shadow**: they submit prices and are scored one-on-one against a paired
   student's real prices (that student isn't scored twice). Nobody sits out.
4. Each round: students enter a price and an optional "why this price?". It
   **auto-saves as they type**, so a typed-but-unsent price is never scored as a
   no-show; they can keep changing it until the round closes. The rival is shown
   only as "the other firm". An optional **round timer** shows a countdown on
   every screen.
5. Instructor watches the market grid fill (it updates live as students submit
   *and* as they edit), then **Close & score round → Next round**.
   - Missing a price → that student is scored **out of the market** ($0; the
     rival gets the sole-seller outcome). Tick **carry forward** first to reuse
     their previous-round price instead.
   - Set a **timer** (seconds) on the create screen or the round screen. A big
     countdown shows on every screen; at 0:00 the round **closes and scores
     itself**, then waits for you to click *Next round* (needs the admin tab
     open). Tick **also advance automatically** to skip that pause — handy in the
     last few rounds.
   - **Hide names** puts "Firm A / B" on the projector and debrief instead.
6. After the last round: leaderboard, then the **Debrief** tab.

Put `?mode=display` on the projector for a live big-screen view (round, timer,
submission count, leaderboard, and a chart). The **Projector view** setting
chooses the chart — *Summary* (one average-price line, the default), *Scoreboard
only*, or *Detail* (per-market paths; busy past ~4 teams).

Other admin tabs:

- **History** — every session is saved automatically. Open a past one, rename it,
  delete a false start, download its CSV, or tick several sessions and
  **Download a class bundle** (ZIP of tidy CSVs, or a multi-tab `.xlsx`).
- **Debrief** — price paths, average price, cumulative profit, a best-response
  explorer, auto-flagged "teachable moments", and player/market leaderboards with
  a *vs. all-NE* column. Every chart exports a PNG; **Figure pack (ZIP)** exports
  the standard set.

---

## 4. The games

Defined declaratively in [`config/games.yaml`](config/games.yaml):

| id | model | notes |
|---|---|---|
| `bertrand` | homogeneous good, `Q = choke − p` | winner-takes-all, ties split |
| `hotelling_high` | linear city, `t = 1` | genuine differentiation |
| `hotelling_low` | linear city, `t = 0.5` | weaker differentiation |

Edit `rounds`, `pairing`, or the demand `params` (`V`, `t`, `c`, `M`, `choke`)
there; `ne` / `collusive` / `monopoly` benchmarks are computed from the model, so
the reference lines on the charts can't drift from the game. Add a game by adding
a `games:` entry (and, for a new demand model, a function in `R/engine/`).

---

## 5. Testing with bots

Two helper scripts let you exercise the whole thing without a class.

### Seed demo data

```sh
Rscript scripts/seed_demo.R
```

Wipes the database and fills it with **two classes × three games** (6 finished
sessions, 48 students) using behavioural archetypes per market. Good for
screenshots and for poking at the Debrief / History / export screens. Reproducible.

### Live play-test — you drive admin, the bot drives the students

Two terminals:

```sh
# terminal 1 — the app
Rscript scripts/run_app.R

# terminal 2 — the students  (START THIS BEFORE you click "Start game")
Rscript scripts/playtest_bot.R
```

Then, in the admin browser: **New session → pick a game → Create.** The bot's
students appear in the lobby within ~1 s; click **Start game**. Each round the
"N of 8 submitted" counter ticks up over ~15 s as the bot submits (with
rationales, visible in the grid and in terminal 2). Score and advance whenever
you like; the bot keeps up across as many games as you run back-to-back.
`Ctrl-C` terminal 2 when done.

Keep the play-test off your real data by pointing both at a scratch file:

```sh
PRICING_DB=data/playtest.sqlite Rscript scripts/run_app.R
PRICING_DB=data/playtest.sqlite Rscript scripts/playtest_bot.R
```

#### Playing a student yourself

Leave your name out of the bot's roster and play that student in a browser tab:

```sh
PLAYTEST_NAMES="Boaz,Cleo,Dmitri,Esme,Farid,Gwen,Hugo" Rscript scripts/playtest_bot.R
```

Open a normal window at `http://127.0.0.1:3838/` and join as "Ada". The bot never
touches a player it doesn't control and skips anyone who has already submitted,
so your manual entries stand.

Speed knobs: `PLAYTEST_MINDELAY` / `PLAYTEST_MAXDELAY` (seconds before each
student submits — default `2` / `18`; try `1` / `4` for a fast run).

### How the bots work

Both scripts talk to the game through the same `sl_*` functions in
`R/session_logic.R` that the app uses — they write `submissions` rows to the
SQLite file, nothing more. They do **not** drive a browser or speak to Shiny.

- **`seed_demo.R`** is a batch generator. Per market it draws an archetype
  (`race` / `collude` / `best_response` / `anchored`), pre-computes the full price
  matrix (`sim_prices()`), then runs `sl_submit → sl_score_round →
  sl_advance_round` to the end, with a ~2 % chance any student skips a round.

- **`playtest_bot.R`** is reactive. It polls the active session twice a second:
  - new session in `lobby` → `sl_add_player()` for each controlled name;
  - phase `open`, new round → schedule each student a random submit time
    `now + runif(MINDELAY, MAXDELAY)` (this is what makes the counter tick up
    gradually);
  - a student's time is up and they haven't submitted → `decide()` a price and
    `sl_submit()` it. `decide()` is the same archetype logic, but *reactive*: it
    reads the rival's most recent price and responds. Archetype is a stable hash
    of the name, so a student behaves consistently within a game;
  - phase `scored` / `finished` → wait for the admin.

  The bot reacts to *your* admin clicks; it never advances the game itself.
  Because it only exercises `sl_submit` downward, it does not test the player
  *UI* (join form, price widget) — that needs a real browser.

WAL mode + a 5 s busy timeout (`db_connect`) let the bot write while the app
reads the same file.

---

## 6. Configuration

Environment variables (all optional):

| var | default | used by |
|---|---|---|
| `PRICING_DB` | `data/pricing_game.sqlite` | app, both bots |
| `PRICING_ADMIN_KEY` | `admin` | app |
| `PRICING_GAMES` | `config/games.yaml` | app |
| `PRICING_PORT` | `3838` | `scripts/run_app.R` |
| `PRICING_DEV` | off | `run_app.R` — `1` turns on hot-reload (re-sources the app + refreshes every browser on save; **dev only**) |
| `PLAYTEST_NAMES` | 8-name roster | `playtest_bot.R` |
| `PLAYTEST_MINDELAY` / `PLAYTEST_MAXDELAY` | `2` / `18` | `playtest_bot.R` |

The SQLite file (`data/`) is git-ignored. Stopping and restarting the app resumes
every session exactly.

---

## 7. Project layout

```
app.R                 Shiny entry point (routes ?role=admin vs student)
config/games.yaml     game / treatment definitions
R/
  engine/             pure demand models, benchmarks, best response  (no Shiny)
  db.R                SQLite schema + helpers
  session_logic.R     sl_*  — the game loop (create/start/submit/score/advance)
  pairing.R           random 2-firm markets
  export.R            sl_export_bundle + ZIP / xlsx writers
  debrief_data.R      dbf_*  — tidy frames + teachable-moment flags
  debrief_plots.R     ggplot builders (shared by the app and debrief.qmd)
  mod_player.R         student view (join, submit, result card)
  mod_admin.R          instructor view: Run / History / Debrief
  mod_display.R        projector view (?mode=display)
debrief.qmd           parametrised report; also writes figs/
scripts/
  run_app.R  run_tests.R  render_figs.R
  seed_demo.R          wipe + fill the DB with demo data
  playtest_bot.R       drive the students during a live play-test
tests/testthat/        227 tests
zzz-2026-output/  zzz-dynamic_pricing_lab/   reference material (prior versions)
```

---

## 8. Tests

```sh
Rscript scripts/run_tests.R
```

Covers the engine (with an exact parity check against the 2026 spreadsheet
results), the game loop, the export bundle, the debrief data/plots, and every
UI screen builder.
