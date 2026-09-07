#!/usr/bin/env Rscript
# Write the standard debrief figure pack (PNGs) for a session, without Quarto.
#
#   Rscript scripts/render_figs.R [session_id] [out_dir]
#
# session_id defaults to the most recently scored session in data/pricing_game.sqlite
# (override the DB path with PRICING_DB). out_dir defaults to figs/session_<id>/.

suppressWarnings(suppressMessages(library(ggplot2)))
source("R/load_engine.R"); source_engine()
source("R/db.R"); source("R/session_logic.R")
source("R/debrief_data.R"); source("R/debrief_plots.R")

args <- commandArgs(trailingOnly = TRUE)
con  <- db_connect(Sys.getenv("PRICING_DB", "data/pricing_game.sqlite"))

sid <- if (length(args) >= 1 && nzchar(args[1])) as.integer(args[1]) else {
  s <- sl_list_sessions(con); s <- s[s$n_result_rows > 0, ]
  if (!nrow(s)) stop("no scored sessions found", call. = FALSE)
  s$session_id[1]
}
out <- if (length(args) >= 2) args[2] else file.path("figs", sprintf("session_%d", sid))

d <- dbf_load(con, sid)
files <- debrief_write_figs(d, out)
cat(sprintf("Session %d (%s) — wrote %d figures to %s/\n",
            sid, d$game$label %||% d$model, length(files), out))
for (f in files) cat("  ", basename(f), "\n")
