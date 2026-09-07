# Build and write the analysis export bundle for one or more sessions.
#
# The bundle is four tidy tables:
#   results_long  one row per player-round, stacked across sessions
#   submissions   the raw price each player entered each round (+ rationale, flags)
#   pairs         who was in which market / firm
#   benchmarks    per session: game params + computed NE / collusive / monopoly
#
# Written as a ZIP of CSVs and/or a multi-tab .xlsx. Needs sl_* (session_logic.R)
# and the engine sourced.

sl_export_bundle <- function(con, session_ids) {
  session_ids <- as.integer(session_ids)
  results <- submissions <- pairs <- benchmarks <- list()

  for (sid in session_ids) {
    s <- sl_session(con, sid)
    if (is.null(s)) next
    g  <- resolve_benchmarks(sl_game(s))
    p  <- g$demand$params
    tag <- data.frame(session_id = sid, game_id = s$game_id,
                      game_label = g$label %||% s$game_id,
                      stringsAsFactors = FALSE)

    results[[length(results) + 1]] <- cbind(tag, db_query(con,
      "SELECT r.round, r.market_id, pl.firm_label, pl.display_name,
              r.price, r.rival_price, r.share, r.quantity, r.revenue,
              r.profit, r.cumulative_profit, r.no_submission
       FROM results r JOIN players pl ON pl.player_id = r.player_id
       WHERE r.session_id = ? ORDER BY r.market_id, pl.firm_label, r.round",
      list(sid)), row.names = NULL)

    submissions[[length(submissions) + 1]] <- cbind(tag, db_query(con,
      "SELECT s.round, pl.market_id, pl.firm_label, pl.display_name,
              s.price, s.rationale, s.carried, s.confirmed, s.submitted_at
       FROM submissions s JOIN players pl ON pl.player_id = s.player_id
       WHERE s.session_id = ? ORDER BY s.round, pl.market_id, pl.firm_label",
      list(sid)), row.names = NULL)

    pairs[[length(pairs) + 1]] <- cbind(tag, db_query(con,
      "SELECT p.market_id, p.firm_label, p.display_name, h.display_name AS shadow_of,
              p.joined_at
       FROM players p LEFT JOIN players h ON h.player_id = p.shadow_of
       WHERE p.session_id = ? AND p.market_id IS NOT NULL
       ORDER BY p.market_id, p.firm_label", list(sid)), row.names = NULL)

    benchmarks[[length(benchmarks) + 1]] <- cbind(tag, data.frame(
      model      = g$demand$model,
      rounds     = s$n_rounds,
      phase      = s$phase,
      V          = p$V          %||% NA,
      t          = p$t          %||% NA,
      L          = p$L          %||% NA,
      M          = p$M          %||% 100,
      mc         = p$mc         %||% 0,
      choke      = p$choke      %||% NA,
      coverage_decay = isTRUE(p$coverage_decay),
      ne         = g$benchmarks$ne        %||% NA,
      collusive  = g$benchmarks$collusive %||% NA,
      monopoly   = g$benchmarks$monopoly  %||% NA,
      created_at = as.POSIXct(s$created_at, origin = "1970-01-01"),
      stringsAsFactors = FALSE))
  }

  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  list(
    results_long = bind(results),
    submissions  = bind(submissions),
    pairs        = bind(pairs),
    benchmarks   = bind(benchmarks)
  )
}

write_bundle_zip <- function(bundle, zip_path) {
  dir <- tempfile("pg_bundle_"); dir.create(dir)
  files <- character(0)
  for (nm in names(bundle)) {
    f <- file.path(dir, paste0(nm, ".csv"))
    utils::write.csv(bundle[[nm]], f, row.names = FALSE, na = "")
    files <- c(files, f)
  }
  readme <- file.path(dir, "README.txt")
  writeLines(c(
    "Pricing game export bundle",
    "",
    "results_long.csv  one row per player per round (price, rival price, share,",
    "                  quantity, revenue, profit, cumulative_profit, no_submission)",
    "submissions.csv   the raw price each player entered each round; carried = 1",
    "                  (carried forward), 2 (scored as no-show)",
    "pairs.csv         who was in each market / firm",
    "benchmarks.csv    per session: game params and the NE / collusive / monopoly prices"
  ), readme)
  files <- c(files, readme)
  zip::zip(zip_path, files = basename(files), root = dir)
  zip_path
}

write_bundle_xlsx <- function(bundle, xlsx_path) {
  writexl::write_xlsx(bundle[c("results_long", "submissions", "pairs", "benchmarks")],
                      path = xlsx_path)
  xlsx_path
}
