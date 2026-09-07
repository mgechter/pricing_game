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
`auto_advance`. Set on `round_started_at` whenever a round opens
(`sl_start_game`, `sl_advance_round`).

- `sl_set_timer(con, sid, seconds, auto_advance=)` and `sl_set_anonymous(con, sid, x)`.
- `sl_time_left(session_row)` → seconds left, or `NA` if no timer / not open.
- The countdown renders in its own `textOutput` on the player, admin and
  projector screens — **independent of the debounced screen render**, so it never
  disturbs an in-progress price entry.
- `auto_advance`: while the admin tab is open, an `observe(invalidateLater(2000))`
  calls `sl_should_auto_advance()` and, on expiry, scores + advances once per
  round (guarded by a `last_auto` reactiveVal).

Set the timer on the create screen or change it live on the round screen.

## Anonymity toggle

`sessions.anonymous`. When on, the projector and the Debrief tab show
`Market N · Firm A/B` instead of student names (the plots call
`plot_price_paths(d, anon = TRUE)`). Players never saw rival names either way.
The Debrief tab's own toggle defaults from the session's setting.

## Projector page — `?mode=display`

`R/mod_display.R`. Read-only, polls the DB like the other views. Shows the game
label, a big countdown, round / phase, "N of M submitted", and once round 1 is
scored: the price-path chart (respecting the anon flag) and a cumulative-profit
leaderboard. `app.R` routes `?mode=display` → `display_server`.

## Migrations

`db_init()` adds the new columns to existing databases
(`players.shadow_of`, `sessions.round_started_at / round_seconds / auto_advance /
anonymous`).

246 tests.
