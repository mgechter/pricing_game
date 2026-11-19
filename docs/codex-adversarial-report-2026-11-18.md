# Codex's adversarial review of the fork, 2026-11-18

Codex's adversarial review of the fork at `47bd021`, 2026-11-18.
Its 16 findings are fixed in `ebd625a`, `9339343` and `c31229f`; one race remains open: a Submit clicked on a screen not yet redrawn for the next round (under a second).
Its tests are in this repo as `tests/testthat/test-review-defects.R`, `test-review-guards.R` and `test-slide-figures.R`, renamed from the `test-codex-*` names below, and the `codex-scratch/` logs it cites were not kept.
C1 is overstated as ranked: a cleared price box reaches R as NA, which the code handled, and a browser check left the student connected; by code reading, the error was reachable only while the box had never been drawn.

The report as Codex wrote it follows.

---

Adversarial review of `47bd021`, performed locally on 2026-11-18. No application code was changed. The original suite passed all 419 checks before the additions. There are 16 intentionally failing reproductions in `tests/testthat/test-codex-adversarial.R` and 18 passing guard tests in `tests/testthat/test-codex-guards.R`.

Findings below are ranked by likely impact in Thursday's class, rather than by ID. **“Crash” includes a failed render/download or a disconnected browser session; no R worker-process termination was reproduced.** The shared-worker deployment makes synchronous work relevant, but these tests do not establish that a browser-session error kills other sessions.

1. **C1 — Clearing the price field disconnects the student. Severity: crash. Thursday: yes, ordinary price editing.**

   Location: `R/mod_player.R:96`; the same missing length check exists in the deadline observer at `R/mod_player.R:189`. A blank numeric input arrives as `NULL`; `as.numeric(NULL)` is length zero, so the `if` condition errors with “missing value where TRUE/FALSE needed.” The exception escapes the observer. The reproduction saves $20, clears the field, advances the debounce, and finds `session$isClosed()` is `TRUE`. A separate scratch probe also reproduced this when the blank price input first appears and when the timer runs while predictions are pending.

   Failing test: **C1**, `tests/testthat/test-codex-adversarial.R:22`.

   Suggested fix: validate that the input is exactly one finite number before testing its value, in both observers. Treat a blank field as an unfinished edit; retain the previous saved price where intended.

2. **C3 — A pending draft can be saved under the next round. Severity: wrong number / misattributed input. Thursday: yes, a timing-dependent phone/round-transition race.**

   Location: `R/mod_player.R:89` and `R/mod_player.R:103`. The debounced value contains only price and rationale. Its handler looks up the current session and round when it eventually fires. If the instructor scores and advances during the 1.2-second debounce, and the phone has not yet acknowledged the replacement input, a round-1 $20 draft becomes a round-2 submission. The test verifies a spurious round-2 database row, including the round-1 rationale; it does not claim the race happens on every transition.

   Failing test: **C3**, `tests/testthat/test-codex-adversarial.R:46`.

   Suggested fix: associate each input/draft with the session, round and player that produced it; discard pending work when that identity changes. Apply the same identity check to Submit.

3. **C2 — Carry-forward errors if the student also no-showed in the previous round. Severity: crash, scoring operation. Thursday: yes, consecutive no-shows with carry enabled.**

   Location: `R/session_logic.R:333`. `.prev_price()` returns the previous result's `NA` price. `.effective_price()` checks only for `NULL`, then inserts `NA` into the non-null `submissions.price` column. Scoring raises `NOT NULL constraint failed: submissions.price`; the round remains open. This is distinct from a valid carry-forward, which passes G9.

   Failing test: **C2**, `tests/testthat/test-codex-adversarial.R:36`.

   Suggested fix: carry only a finite previous price. Otherwise use the normal no-submission outcome. Make scoring transactional so an error cannot leave partially written markets.

4. **C7 — The displayed Bertrand best response recommends a tie instead of an undercut. Severity: wrong number. Thursday: yes, normal Bertrand prices below or at $50.**

   Location: `R/debrief_plots.R:241`, with the approximate solver in `R/engine/equilibria.R:90`; flag text uses the same rounding at `R/debrief_data.R:125`. Against $20, the numerical answer is about $19.99992, but the plot labels it **“Best response $20.00.”** At the displayed price the firm earns $800, while the feasible $19.99 price earns $1,599.3999. This is economically consequential rounding at a discontinuity, not a harmless numerical tolerance.

   Failing test: **C7**, `tests/testthat/test-codex-adversarial.R:103`; it reads the actual plot label and compares the corresponding payoffs.

   Suggested fix: choose and enforce a price increment, then solve and display on that grid; or describe the continuous Bertrand response explicitly as “just below $20,” without labelling $20 itself as optimal. Keep flags and plot markers consistent.

5. **C6 — The Debrief market leaderboard adds the shadow's profit. Severity: wrong number. Thursday: yes, any odd roster with a profitable shadow.**

   Location: `R/debrief_data.R:344`. `dbf_leaderboards()` sums every result in a market. In a three-player, one-round $20 Bertrand tie it reports $2,400 instead of A+B's $1,600. The new `dbf_market_ranks()` correctly reports $1,600, so the tab's market leaderboard can contradict its own One market figure and the Slide figures rankings.

   Failing test: **C6**, `tests/testthat/test-codex-adversarial.R:94`.

   Suggested fix: filter market totals to Firm A and Firm B, preferably sharing the calculation with `dbf_market_ranks()`. Keep the shadow on the individual leaderboard.

6. **C4 — Re-scoring changes the database but leaves the projector's profits stale. Severity: wrong number. Thursday: yes, using the offered carry-forward re-score button.**

   Location: `R/mod_display.R:39`. The projector cache key includes the phase, round and result-row count, but no result revision/time. Re-scoring keeps all of those fields unchanged. The reproduction changes round-2 profits from $1,600/$0 to $800/$800; the projector data still shows $1,600/$0 after polling. The error persists until another key-changing event, such as advancing the round.

   Failing test: **C4**, `tests/testthat/test-codex-adversarial.R:62`.

   Suggested fix: include the scoring revision or latest `computed_at` in the projector cache key.

7. **C10 — “Both firms” raises a render error when either rival price is missing. Severity: crash, render/download. Thursday: yes, inspecting a market with a no-show.**

   Location: `R/debrief_plots.R:199`, called without validation by `R/mod_admin.R:292`. A valid scored market with one no-show reaches `.br_panel()` and raises “that market / round isn't available.” The explorer's Both firms option cannot render; its download calls the same builder. Default single-firm exploration also fails if the selected firm's rival no-showed.

   Failing test: **C10**, `tests/testthat/test-codex-adversarial.R:141`; exercises the actual Shiny render and permits a handled Shiny validation message.

   Suggested fix: show a clear no-data/sole-seller state, or draw the corresponding sole-seller payoff with an explicit no-submission label. Validate availability before rendering/downloading.

8. **C16 — “Show as Firm A / B” still displays students' names in the tables. Severity: wrong label. Thursday: yes, showing Debrief live with names hidden.**

   Location: `R/mod_admin.R:309`, `R/mod_admin.R:318`, `R/mod_admin.R:326`. The checkbox controls plots, but the Teachable moments and both leaderboards still render the original names. Checking it leaves “Student 1” in the rendered table HTML. This differs from C11: the leak occurs while the checkbox remains selected.

   Failing test: **C16**, `tests/testthat/test-codex-adversarial.R:210`.

   Suggested fix: apply the selected anonymity setting to all displayed names and flag prose, using market plus firm to distinguish students. Audit the prediction table and Figure pack under the same setting.

9. **C11 — Refresh resets the name-hiding checkbox. Severity: wrong label / stale UI choice. Thursday: yes, refreshing Debrief while projecting it.**

   Location: `R/mod_admin.R:673`. The rebuilt checkbox takes its value from the session's original `anonymous` setting, not the instructor's current choice. With a normal non-anonymous session, checking “Show as Firm A / B” and clicking Refresh emits an unchecked checkbox. The browser will send the reset value back, exposing plot names again.

   Failing test: **C11**, `tests/testthat/test-codex-adversarial.R:156`; inspects the replacement checkbox HTML.

   Suggested fix: preserve the instructor's current selection across renders, as the new market pickers already attempt to do.

10. **C12 — An in-progress leaderboard compares actual profit with unplayed rounds. Severity: wrong number. Thursday: yes, opening Debrief before finishing or ending early.**

    Location: `R/debrief_data.R:331` and `R/debrief_data.R:340`. In hotelling_high, after one scored round at the exact $100 Nash price and with round 2 open, each firm has earned the correct $5,000. The “vs. all-NE” column says **−$45,000**, because it subtracts ten rounds of benchmark profit. The appropriate comparison over the one scored round is $0. Early ending fixes the denominator, but ordinary in-progress use does not.

    Failing test: **C12**, `tests/testthat/test-codex-adversarial.R:168`.

    Suggested fix: compare actual and benchmark profit over the same scored rounds, or explicitly separate a full-horizon target from the achieved-to-date comparison.

11. **C5 — A reset or another one-round game can leave the timer unable to score round 1. Severity: wrong label / stale round state. Thursday: conditional, timer plus a one-round early end/do-over.**

    Location: `R/mod_admin.R:363`. `last_auto` remembers only the round number and survives game resets/session changes. After automatically scoring round 1, ending early and resetting, the replay reaches an expired timer but stays `open`: `last_auto()` is still 1. The test proves the original timer scored successfully before testing the replay.

    Failing test: **C5**, `tests/testthat/test-codex-adversarial.R:76`.

    Suggested fix: key the auto-score latch by session plus round instance/start time, and clear it on reset. Review the player's analogous `auto_done` latch too.

12. **C9 — Debrief can show the discarded pre-reset run after a replay. Severity: wrong number. Thursday: conditional, using Reset after having opened Debrief.**

    Location: `R/mod_admin.R:218` and `R/mod_admin.R:228`. The loaded data depend only on the selected session ID and explicit Refresh counter. Reopening the tab, reset, replay and deletion do not invalidate them. A one-round $20 game is opened in Debrief, reset through the admin handler, replayed at $40, then reopened: the cached prices remain $20. The explicit Refresh button repairs this instance; simply reopening the tab does not.

    Failing test: **C9**, `tests/testthat/test-codex-adversarial.R:124`.

    Suggested fix: invalidate/reload when entering Debrief and when the selected session is reset, deleted or rescored; reconcile the selected session against the current scored-session list.

13. **C14 — The last-round-cut caption calls a filtered selection the two biggest cuts. Severity: wrong label. Thursday: yes, mixed market histories in hotelling_low.**

    Location: `R/debrief_data.R:232` and `R/slide_figures.R:91`. Selection deliberately prioritizes markets whose two previous prices exceeded NE. The caption and title instead claim the biggest cuts overall. The reproduction has qualifying cuts of $10 and $5, and another market with an actual $200 cut whose rival previously priced below NE. The slide claims the biggest cuts were $10 and $5 and omits $200.

    Failing test: **C14**, `tests/testthat/test-codex-adversarial.R:182`.

    Suggested fix: qualify the title/caption as cuts following both firms pricing above NE, including a clear fallback explanation; alternatively select the actual largest cuts.

14. **C8 — A game scored with no submissions breaks the Debrief Figure pack. Severity: crash, render/download. Thursday: conditional, scoring before anyone submits.**

    Location: `R/debrief_plots.R:151`, called by `R/debrief_plots.R:341`; quartiles also reject an empty finite-price sample at `R/debrief_plots.R:94`. Scoring both firms as no-shows is supported and creates results. `plot_avg_price()` then aggregates zero rows and raises “no rows to aggregate,” aborting the pack. The average-price render/projector path has the same empty-data problem. The Slide figures ZIP handles this case successfully (G14).

    Failing test: **C8**, `tests/testthat/test-codex-adversarial.R:116`.

    Suggested fix: distinguish scored zero-profit/no-price results from no game. Emit a labelled empty plot or omit unavailable figures with an explanation, without aborting the download.

15. **C15 — Missing final prices make a two-round game say it had one round. Severity: wrong label. Thursday: conditional, no comparable final-round cuts.**

    Location: `R/slide_figures.R:88`. `dbf_last_round_cuts()` returns no rows both for a one-round game and for a multi-round game without finite final price differences. The caller assumes the former. After a $150 round followed by both firms no-showing, the source says “2 rounds scored” but the last-round-cuts note says “the game has one round.”

    Failing test: **C15**, `tests/testthat/test-codex-adversarial.R:202`.

    Suggested fix: check the actual scored-round count separately and explain missing comparable prices for multi-round games.

16. **C13 — Unchanged profits are presented as teachable “Profit swing” cases. Severity: wrong label. Thursday: yes, stable/equal-price paths.**

    Location: `R/debrief_data.R:104`. Zero changes survive the filter and the top-N selection. At the exact Hotelling NE in every round, the flags claim that a student's profit “jumped $0.00.” These are not actual swings and occupy the flag picker.

    Failing test: **C13**, `tests/testthat/test-codex-adversarial.R:175`.

    Suggested fix: remove zero/negligible profit changes before ranking; use an explicit display-precision/materiality tolerance.

**Attacks that held — passing regression guards.** Each row is one test in `tests/testthat/test-codex-guards.R`.

| Guard | Verified behavior |
| --- | --- |
| G1 | Duplicate names, including case variants, are rejected. Quotes, commas, pipes, HTML, emoji and a long name preserve database identity; HTML is escaped; market IDs remain parseable and plot grobs build. This is not a claim that arbitrary-length text fits every image. |
| G2 | Slide data choose the latest **scored** session, ignore a newer unscored game/open-round submissions, and fall back correctly after reset/deletion. |
| G3 | One-round early ending changes the horizon and source caption correctly, omits absent session labels, handles the unavailable cut figure, and reset restores ten rounds while keeping player tokens. |
| G4 | The **new** market ranks exclude a profitable shadow and break equal-profit ties deterministically by market ID; the market-detail subtitle has the correct A+B total. |
| G5 | Quartiles equal hand-calculated values for missing, carried and shadow prices; an open unscored round is excluded. |
| G6 | All prices equal to zero give three zero quartiles and a drawable one-round plot. |
| G7 | Rival case finders/added flags select the correct market, round and focal firm, exclude shadows/no-show rivals, give the correct gap, and mark the nearest case as non-exact. |
| G8 | Last-round differences match firm labels despite reversed row order, exclude the shadow's much larger change, and use the early-ending round. |
| G9 | Carrying **finite** prior prices works; cumulative profits and shadow-host scoring are correct, including a missing host. |
| G10 | Both no-shows yield zero profit; prices at/above the choke or reservation value yield zero profit; zero-price ties split quantity correctly in all three configured games. |
| G11 | Both Hotelling models' numerical best-response profits agree with independently evaluated analytic kink/stationary-point candidates, including rivals at 0, V and above V. |
| G12 | Anonymous plot labels remove names while preserving firm order, actual rival prices, market and round in the paired best-response plot. |
| G13 | The Slide ZIP contains each available figure and market under unique correct paths, with nonempty PNGs and correct session provenance in captions. |
| G14 | An all-no-show game does not abort the Slide figures writer; unavailable rival/quartile figures are skipped. |
| G15 | Required predictions gate all players, including the shadow; completing them or the actual admin force handler releases the gate; disabled predictions do not gate. |
| G16 | A joined student's lobby count updates as others join, and price/result formatting preserves cents. |
| G17 | Thirty students' serial edits on one SQLite connection preserve the latest per-player/per-round prices and sticky Submit confirmations across three rounds. |
| G18 | The Debrief market selections and Both firms plot refer to the chosen market/round; all three actual mock-session PNG download handlers produce files; market choices survive Refresh. |

The review used direct calls, actual ggplot rendering/writing, and `shiny::testServer()`. It did not use the network or a running browser server. G17 verifies serialized persistence, not a 200-browser load test. A few scratch exploration scripts/logs are retained under `codex-scratch/`; the only additions outside that directory are this report and the two requested test files. No files under `R/`, `app.R`, `config/`, `scripts/` or `manifest.json` were changed.

The final command is `Rscript scripts/run_tests.R`, with `TESTTHAT_MAX_FAILS=Inf` in its environment so intentional failures do not limit the run. Exit status 1 is expected. Full output: `codex-scratch/full-tests.log`. Final summary follows:

```text
══ Results ═════════════════════════════════════════════════════════════════════
Duration: 38.9 s

[ FAIL 16 | WARN 0 | SKIP 0 | PASS 560 ]

110 test(s): 16 failed, 0 errored
```
