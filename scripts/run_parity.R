#!/usr/bin/env Rscript
# Re-run the 2026 submitted prices through the engine and compare per-player
# profit totals to the instructor's hand-built all_profits.csv.
# From the project root:  Rscript scripts/run_parity.R
#
# Findings: docs/phase0-parity-findings.md

source("R/load_engine.R"); source_engine()
source("R/parity.R")

cat("== Parity vs 2026 all_profits.csv ==\n")
cat("   Hotelling params overridden to the 2026 sheet: t (O3) = 2, capture_gap = 100.\n\n")
cmp <- run_parity()
print_parity(cmp)

cat("\n-- by game --\n")
for (g in c("bertrand", "hotelling_high", "hotelling_low")) {
  s <- cmp[cmp$game_id == g, ]
  cat(sprintf("  %-15s  max|diff| = %8.2f   RMS pct = %.3f%%\n",
              g, max(abs(s$diff)), sqrt(mean(s$pct_diff^2))))
}

cat("\n== Same prices under the AS-DESIGNED params (t = 1 / t = 0.5) ==\n\n")
des <- run_parity(as_designed = TRUE)
print_parity(des)
cat("\nThe differentiated games were meant to differ in t; the 2026 sheet left\n")
cat("O3 = 2 in both, so 'high' and 'low' differentiation were the same game.\n")
