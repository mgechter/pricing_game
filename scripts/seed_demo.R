#!/usr/bin/env Rscript
# Wipe the database and fill it with two classes' worth of play — for demos,
# screenshots, and exercising the Debrief tab / export. Reproducible, idempotent.
#
#   Rscript scripts/seed_demo.R
#   PRICING_DB=/tmp/demo.sqlite Rscript scripts/seed_demo.R

set.seed(2026)
suppressWarnings(suppressMessages({
  source("R/load_engine.R"); source_engine()
  source("R/db.R"); source("R/pairing.R"); source("R/session_logic.R")
}))

DB <- Sys.getenv("PRICING_DB", "data/pricing_game.sqlite")
unlink(paste0(DB, c("", "-wal", "-shm")))
dir.create(dirname(DB), showWarnings = FALSE, recursive = TRUE)
con <- db_connect(DB); db_init(con)
on.exit(DBI::dbDisconnect(con))

GAMES <- c("bertrand", "hotelling_high", "hotelling_low")
ROSTERS <- list(
  "Section A (MW 10am)" = c("Ada", "Boaz", "Cleo", "Dmitri", "Esme", "Farid", "Gwen", "Hugo"),
  "Section B (TR 2pm)"  = c("Ines", "Jae", "Kira", "Liam", "Mira", "Noor", "Omar")   # odd -> a shadow
)
RATIONALES <- c(
  "Undercutting to steal share", "Trying to hold the line at a high price",
  "Matching my rival's last move", "Testing whether they'll follow me up",
  "Best response to what they did last round", "Going for the monopoly price",
  "They keep cutting, so I have to", "Splitting the difference", "")

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# One market's 10 (or n) rounds of prices, by behavioural archetype.
sim_prices <- function(model, params, bm, nr, archetype) {
  bert <- model == "bertrand_homogeneous"
  ne   <- bm$ne
  mono <- bm$collusive
  lo   <- if (bert) 0 else max(0, ne - 60)
  hi   <- if (bert) 65 else mono + 25
  br   <- function(pr) best_response(model, params, pr)$price

  p <- matrix(NA_real_, nr, 2)
  p[1, ] <- round(if (bert) runif(2, 12, 45) else runif(2, ne - 15, mono - 10), 1)

  for (r in 2:nr) {
    for (i in 1:2) {
      j <- 3 - i
      own <- p[r - 1, i]; riv <- p[r - 1, j]
      nx <- switch(archetype,
        race          = riv * runif(1, 0.55, 0.9) - if (bert) runif(1, 0, 3) else 0,
        collude       = mono - abs(mono - own) * runif(1, 0.3, 0.7) + rnorm(1, 0, mono * 0.03),
        best_response = 0.55 * br(riv) + 0.45 * own + rnorm(1, 0, ne * 0.08 + 3),
        anchored      = if (i == 1) ne + rnorm(1, 0, 4)
                        else 0.6 * br(riv) + 0.4 * own + rnorm(1, 0, 5),
        own)
      p[r, i] <- round(clamp(nx, lo, hi), 1)
    }
    # a colluding pair sometimes has one firm defect for a round
    if (archetype == "collude" && nr >= 4 && r >= 3 && runif(1) < 0.18)
      p[r, sample(2, 1)] <- round(clamp(br(mono) * runif(1, 0.7, 0.95), lo, hi), 1)
  }
  p
}

play_session <- function(class_label, game_id, roster) {
  g0    <- resolve_benchmarks(get_game(game_id))
  label <- sprintf("%s — %s", class_label, g0$label)
  sid   <- sl_create_session(con, game_id, label = label)
  for (nm in roster) sl_add_player(con, sid, nm)
  sl_start_game(con, sid)                                  # random pairing

  s  <- sl_session(con, sid)
  g  <- resolve_benchmarks(sl_game(s))
  nr <- s$n_rounds
  pl <- sl_players(con, sid); pl <- pl[!is.na(pl$market_id), ]
  markets <- sort(unique(pl$market_id))

  pool <- switch(game_id,
    bertrand       = c("race", "race", "collude", "anchored"),
    hotelling_high = c("collude", "best_response", "best_response", "anchored"),
    hotelling_low  = c("best_response", "race", "collude", "best_response"))
  arch <- sample(rep_len(pool, length(markets)))

  book <- lapply(markets, function(mk)
    sim_prices(g$demand$model, g$demand$params, g$benchmarks, nr,
               arch[match(mk, markets)]))
  names(book) <- as.character(markets)
  # a shadow gets its own (best-response-ish) price column in its host's market
  for (mk in unique(pl$market_id[!is.na(pl$shadow_of)]))
    book[[as.character(mk)]] <- cbind(book[[as.character(mk)]],
      sim_prices(g$demand$model, g$demand$params, g$benchmarks, nr, "best_response")[, 1])

  for (r in seq_len(nr)) {
    for (mk in markets) {
      firms <- pl[pl$market_id == mk, ]
      firms <- firms[order(firms$firm_label), ]            # Firm A, Firm B, [Shadow]
      pm <- book[[as.character(mk)]]
      for (i in seq_len(nrow(firms))) {
        if (runif(1) < 0.02) next                          # ~2% forget a round
        sl_submit(con, sid, firms$player_id[i], r, pm[r, i],
                  rationale = sample(RATIONALES, 1), confirm = TRUE)
      }
    }
    sl_score_round(con, sid, carry_forward = FALSE)
    sl_advance_round(con, sid)
  }

  tot <- sum(db_query(con,
    "SELECT profit FROM results WHERE session_id = ?", list(sid))$profit)
  sprintf("  #%d  %-46s  %2d rounds  total profit $%s", sid, label, nr,
          formatC(tot, format = "f", digits = 0, big.mark = ","))
}

cat("Cleared", DB, "\nSeeding two classes x three games...\n\n")
for (cls in names(ROSTERS))
  for (gm in GAMES)
    cat(play_session(cls, gm, ROSTERS[[cls]]), "\n")

cat(sprintf("\nDone. %d sessions, %d players, %d result rows.\n",
    db_query(con, "SELECT COUNT(*) n FROM sessions")$n,
    db_query(con, "SELECT COUNT(*) n FROM players")$n,
    db_query(con, "SELECT COUNT(*) n FROM results")$n))
