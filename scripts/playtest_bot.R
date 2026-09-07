#!/usr/bin/env Rscript
# Live play-test helper. You run the app + drive the admin browser; this script
# plays the students by writing submissions to the same SQLite file. The admin
# grid updates within ~1s (the app polls the DB), so it looks and feels like a
# real class.
#
#   1.  Rscript scripts/run_app.R                    # in one terminal
#   2.  Rscript scripts/playtest_bot.R               # in another — START THIS FIRST,
#                                                    #   before you click "Start game"
#   3.  In the admin browser: New session -> pick a game -> Create.
#       The bot's players appear in the lobby. Click "Start game".
#       Each round: watch the grid fill, then "Close & score round" -> "Next round".
#   4.  The bot keeps up across as many games as you run. Ctrl-C to stop.
#
# Options (env vars):
#   PRICING_DB       sqlite path                (default data/pricing_game.sqlite)
#   PLAYTEST_NAMES   comma list of students     (default a roster of 8)
#   PLAYTEST_MINDELAY / PLAYTEST_MAXDELAY  seconds a student waits before submitting
#                                              (default 2 / 18)
#
# To play one student yourself in a browser tab, just leave that name out of
# PLAYTEST_NAMES — the bot never touches a player it doesn't control, and skips
# any player who already submitted this round.

suppressWarnings(suppressMessages({
  source("R/load_engine.R"); source_engine()
  source("R/db.R"); source("R/session_logic.R")
}))

DB     <- Sys.getenv("PRICING_DB", "data/pricing_game.sqlite")
NAMES  <- strsplit(Sys.getenv("PLAYTEST_NAMES",
            "Ada,Boaz,Cleo,Dmitri,Esme,Farid,Gwen,Hugo"), "\\s*,\\s*")[[1]]
DMIN   <- as.numeric(Sys.getenv("PLAYTEST_MINDELAY", "2"))
DMAX   <- as.numeric(Sys.getenv("PLAYTEST_MAXDELAY", "18"))

if (!file.exists(DB)) stop("no database at ", DB, " — start the app first", call. = FALSE)
con <- db_connect(DB)

archetype <- function(nm) c("undercut", "collude", "best_response", "anchored")[
  (sum(utf8ToInt(nm)) %% 4) + 1]

# A student's price this round, reacting to the rival's most recent price.
decide <- function(model, params, bm, nm, round, own_prev, rival_prev) {
  bert <- model == "bertrand_homogeneous"
  ne <- bm$ne; mono <- bm$collusive
  lo <- if (bert) 0 else max(0, ne - 50); hi <- if (bert) 70 else mono + 25
  if (is.na(own_prev))
    return(round(if (bert) runif(1, 12, 45) else runif(1, ne - 10, mono - 15), 1))
  br <- function(pr) best_response(model, params, pr)$price
  nx <- switch(archetype(nm),
    undercut      = (if (bert) rival_prev - runif(1, 1, 6) else 0.5 * br(rival_prev) + 0.5 * own_prev - runif(1, 0, 8)),
    collude       = mono - abs(mono - own_prev) * runif(1, 0.25, 0.6) + rnorm(1, 0, mono * 0.03),
    best_response = 0.6 * br(rival_prev) + 0.4 * own_prev + rnorm(1, 0, ne * 0.07 + 3),
    anchored      = ne + rnorm(1, 0, 5))
  round(min(max(nx, lo), hi), 1)
}

RATIONALES <- c("Undercutting to steal share", "Holding the line", "Matching my rival",
  "Best response to last round", "Going for the monopoly price", "They keep cutting",
  "Testing if they follow me up", "")

last_price <- function(sid, pid, round) {
  v <- db_one(con, "SELECT price FROM submissions WHERE session_id=? AND player_id=? AND round=?",
              list(sid, pid, round - 1))
  if (!is.null(v)) return(v$price)
  v <- db_one(con, "SELECT price FROM results WHERE session_id=? AND player_id=? AND round=?",
              list(sid, pid, round - 1))
  if (is.null(v)) NA_real_ else v$price
}

cat(sprintf("Play-test bot on %s — controlling: %s\n(Ctrl-C to stop)\n\n",
            DB, paste(NAMES, collapse = ", ")))

seen_session <- -1L
seen_round   <- -1L
schedule     <- list()          # player_id -> epoch time to submit

repeat {
  s <- tryCatch(sl_active_session(con), error = function(e) NULL)
  if (is.null(s)) { Sys.sleep(1); next }

  if (s$session_id != seen_session) {
    seen_session <- s$session_id; seen_round <- -1L; schedule <- list()
    if (s$phase == "lobby") {
      have <- sl_players(con, s$session_id)$display_name
      for (nm in setdiff(NAMES, have))
        tryCatch(sl_add_player(con, s$session_id, nm), error = function(e) NULL)
      cat(sprintf("[#%d] joined %d players — click \"Start game\" when ready.\n",
                  s$session_id, length(setdiff(NAMES, have))))
    } else {
      cat(sprintf("[#%d] already %s — start the bot before Start next time.\n",
                  s$session_id, s$phase))
    }
  }

  if (s$phase == "open") {
    g  <- resolve_benchmarks(sl_game(s))
    rn <- s$current_round
    pl <- sl_players(con, s$session_id)
    pl <- pl[!is.na(pl$market_id) & pl$display_name %in% NAMES, , drop = FALSE]

    if (rn != seen_round) {
      seen_round <- rn
      now <- as.numeric(Sys.time())
      schedule <- stats::setNames(as.list(now + runif(nrow(pl), DMIN, DMAX)), pl$player_id)
      cat(sprintf("[#%d] round %d open — %d students will submit over the next ~%ds\n",
                  s$session_id, rn, nrow(pl), round(DMAX)))
    }

    now <- as.numeric(Sys.time())
    for (i in seq_len(nrow(pl))) {
      pid <- as.character(pl$player_id[i])
      due <- schedule[[pid]]
      if (is.null(due) || now < due) next
      done <- db_one(con, "SELECT 1 FROM submissions WHERE session_id=? AND round=? AND player_id=?",
                     list(s$session_id, rn, pl$player_id[i]))
      if (!is.null(done)) { schedule[[pid]] <- NULL; next }
      rival <- pl[0, ]  # find market rival
      others <- sl_players(con, s$session_id)
      rival_id <- others$player_id[others$market_id == pl$market_id[i] &
                                   others$player_id != pl$player_id[i]]
      price <- decide(g$demand$model, g$demand$params, g$benchmarks,
                      pl$display_name[i], rn,
                      last_price(s$session_id, pl$player_id[i], rn),
                      if (length(rival_id)) last_price(s$session_id, rival_id[1], rn) else NA_real_)
      why <- sample(RATIONALES, 1)
      tryCatch({
        sl_submit(con, s$session_id, pl$player_id[i], rn, price, rationale = why,
                  confirm = TRUE)
        cat(sprintf("   %-8s $%-7s %s\n", pl$display_name[i], price,
                    if (nzchar(why)) paste0("— \"", why, "\"") else ""))
      }, error = function(e) NULL)
      schedule[[pid]] <- NULL
    }
  }

  if (s$phase == "finished" && seen_round != -2L) {
    seen_round <- -2L
    cat(sprintf("[#%d] finished. Open the Debrief tab, or create the next game.\n\n",
                s$session_id))
  }

  Sys.sleep(0.5)
}
