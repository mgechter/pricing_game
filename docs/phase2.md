# Phase 2 — full v1

Projector view, round timer, shadow pairing for odd headcount, anonymity toggle.
Lockstep round progression kept; rotation-aware pairing deferred to Phase 4.

## Shadow pairing (odd headcount)

`make_pairs()` now returns `{pairs, shadow}` instead of `sitting_out`. With an odd
number of players (≥ 3), the last student after the shuffle is attached to a
random already-paired student as a **shadow**:

- `players.shadow_of` = the host's `player_id`; the shadow sits in the host's
  `market_id` with `firm_label = "Shadow"`.
- The shadow submits a price each round like anyone else.
- `sl_score_round()` scores the real 2-firm markets first, then each shadow
  **one-on-one against the host's effective price** for that round (submission →
  carried → out-of-market, same rules). Only the shadow's result row is written —
  the host is never scored twice.
- If the host no-showed (scored out of their own market), the shadow faces an
  absent rival and gets the sole-seller outcome.
- Export `pairs.csv` gains a `shadow_of` column (the host's display name).

## Round timer

New session columns: `round_started_at`, `round_seconds` (0 = off),
`auto_advance`. `round_started_at` is set whenever a round opens
(`sl_start_game`, `sl_advance_round`).

- `sl_set_timer(con, sid, seconds, auto_advance=)`, `sl_set_anonymous(con, sid, x)`.
- `sl_time_left(session_row)` → seconds left, `NA` if no timer / not open.
- **Big countdown** (`countdown_tag()` in `ui_helpers.R`, green → amber → red) on
  the player, admin and projector screens, each in its own `uiOutput` —
  **independent of the debounced screen render**, so ticking never disturbs an
  in-progress price entry.
- **At 0:00 the round auto-closes and scores** (any `round_seconds > 0`), then
  sits in `scored` — the instructor clicks *Next round* when ready, so the clock
  isn't perpetually running. An `observe(invalidateLater(1000))` in the admin
  server calls `sl_should_auto_score()` once per round (guarded by a `last_auto`
  reactiveVal); needs the admin tab open.
- **`auto_advance`** (checkbox "Also advance to the next round automatically") is
  opt-in — it chains `sl_advance_round()` after the auto-score, for continuous
  flow in the last few rounds.

Set the timer on the create screen or change it live on the round screen.

## Anonymity toggle

`sessions.anonymous`. When on, the projector leaderboard and the Debrief tab show
`Market N · Firm A/B` instead of student names. Players never saw rival names
either way. The Debrief tab's own toggle defaults from the session's setting.

## Projector page — `?mode=display`

`R/mod_display.R`. Read-only, polls the DB like the other views. Shows the game
label, a big countdown, round / phase, "N of M submitted", and once round 1 is
scored a chart + a cumulative-profit leaderboard. `app.R` routes `?mode=display`.

`sessions.projector_view` (`sl_set_projector`) picks what shows, from the create
screen or live on the round screen:

| view | chart | notes |
|---|---|---|
| **summary** (default) | average price per round vs NE / monopoly lines | one readable line; no names |
| **scoreboard** | none | leaderboard only |
| **detail** | per-market price paths | busy past ~4 markets; better kept for the debrief |

The leaderboard honours `anonymous`; the summary chart has no names by
construction; `detail` always uses Firm/Market labels.

## Admin round grid

- A **Why** column shows each student's "why this price?" rationale next to Price
  and Profit, and stays visible after scoring (rationale + outcome side by side).
- No standalone Re-score button. On the scored screen, **only when the round had a
  no-show**, a single self-explanatory button appears —
  *"Re-score, carrying the no-shows' last-round prices forward"* — for the case
  where a student was mid-typing when the round closed. A clean round just shows
  "Next round".

## Migrations

`db_init()` adds the new columns to existing databases
(`players.shadow_of`, `sessions.round_started_at / round_seconds / auto_advance /
anonymous`).

246 tests.
