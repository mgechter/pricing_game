# Parity check: re-run the 2026 submitted prices through the engine and compare
# per-player profit totals against the instructor's hand-built all_profits.csv.
#
# The 2026 Google Sheet used O2 = 250 and O3 = 2 in BOTH differentiated games,
# with the full-capture price gap hard-coded at 100. In the engine's parameters
# that is  t = 2 (O3),  L = 100,  M = 100,  V = 250,  mc = 0,  capture_gap = 100.
# `run_parity()` applies these `.sheet_2026` overrides so it reproduces the sheet
# exactly.  run_parity(as_designed = TRUE) uses config/games.yaml as-is (the
# intended t = 1 / t = 0.5 design) instead — useful for seeing how the games were
# meant to differ.
#
# Requires the engine to be sourced first: source("R/load_engine.R"); source_engine()

DATA_DIR_DEFAULT   <- "zzz-2026-output/pricing_game data"
GAMES_PATH_DEFAULT <- "config/games.yaml"

# What the 2026 sheet actually used, by game id. The sheet's share formula had no
# coverage decay, so that is forced off here even though games.yaml now turns it on
# for the rebuilt games.
.sheet_2026 <- list(
  hotelling_high = list(t = 2, L = 100, M = 100, V = 250, mc = 0,
                        capture_gap = 100, coverage_decay = FALSE),
  hotelling_low  = list(t = 2, L = 100, M = 100, V = 250, mc = 0,
                        capture_gap = 100, coverage_decay = FALSE)
)

.parity_map <- data.frame(
  file    = c("pricing_long_bertrand.csv",
              "pricing_long_hoteling1.csv",
              "pricing_long_hoteling5.csv"),
  game_id = c("bertrand", "hotelling_high", "hotelling_low"),
  stringsAsFactors = FALSE
)

.read_long <- function(path) {
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- gsub("[^a-z0-9]+", "_", tolower(names(d)))
  data.frame(
    firm      = as.character(d$name),
    market_id = d$market_id,
    round     = as.integer(d$round),
    price     = as.numeric(d$price),
    stringsAsFactors = FALSE
  )
}

# One row per (player, game) from all_profits.csv, profits as plain numbers.
.read_actual_totals <- function(data_dir) {
  a <- utils::read.csv(file.path(data_dir, "all_profits.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  money <- function(x) as.numeric(gsub("[$,]", "", x))
  long <- rbind(
    data.frame(firm = a[["Student 1"]], game_raw = a[["Game"]],
               actual = money(a[["Student 1 Total Profit"]]), stringsAsFactors = FALSE),
    data.frame(firm = a[["Student 2"]], game_raw = a[["Game"]],
               actual = money(a[["Student 2 Total Profit"]]), stringsAsFactors = FALSE)
  )
  long$game_id <- ifelse(grepl("^Bertrand",   long$game_raw), "bertrand",
                  ifelse(grepl("^Hoteling 1", long$game_raw), "hotelling_high",
                                                              "hotelling_low"))
  long[!duplicated(long[c("firm", "game_id")]), c("firm", "game_id", "actual")]
}

run_parity <- function(data_dir    = DATA_DIR_DEFAULT,
                       games_path  = GAMES_PATH_DEFAULT,
                       as_designed = FALSE) {
  games  <- load_games(games_path)
  actual <- .read_actual_totals(data_dir)

  computed <- do.call(rbind, lapply(seq_len(nrow(.parity_map)), function(i) {
    gid  <- .parity_map$game_id[i]
    game <- games[[gid]]
    if (!as_designed && !is.null(.sheet_2026[[gid]])) {
      game$demand$params <- modifyList(game$demand$params, .sheet_2026[[gid]])
    }
    sub <- .read_long(file.path(data_dir, .parity_map$file[i]))
    res <- resolve_game(game, sub)
    agg <- stats::aggregate(profit ~ firm, data = res, FUN = sum)
    data.frame(firm = as.character(agg$firm), game_id = gid,
               computed = agg$profit, stringsAsFactors = FALSE)
  }))

  cmp <- merge(actual, computed, by = c("firm", "game_id"), all = TRUE)
  cmp$diff     <- cmp$computed - cmp$actual
  cmp$pct_diff <- 100 * cmp$diff / cmp$actual
  cmp[order(cmp$game_id, cmp$firm), ]
}

print_parity <- function(cmp) {
  out <- cmp
  out$actual   <- round(out$actual, 0)
  out$computed <- round(out$computed, 2)
  out$diff     <- round(out$diff, 2)
  out$pct_diff <- round(out$pct_diff, 4)
  print(out, row.names = FALSE)
  cat(sprintf("\n  max |diff| = %.4f      max |pct diff| = %.4f%%\n",
              max(abs(cmp$diff), na.rm = TRUE),
              max(abs(cmp$pct_diff), na.rm = TRUE)))
  invisible(cmp)
}
