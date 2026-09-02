#!/usr/bin/env Rscript
# Launch the pricing game locally.  From the project root:
#
#   Rscript scripts/run_app.R           # port 3838
#   PRICING_ADMIN_KEY=secret Rscript scripts/run_app.R
#
# Then expose it to the class for the hour with a tunnel, e.g.
#   ngrok http 3838
#   tailscale funnel 3838
#
# Player link:  http://<host>:3838/
# Admin link:   http://<host>:3838/?role=admin&key=<PRICING_ADMIN_KEY>   (default key: "admin")

port <- as.integer(Sys.getenv("PRICING_PORT", "3838"))
key  <- Sys.getenv("PRICING_ADMIN_KEY", "admin")

cat(sprintf("\n  Player:  http://127.0.0.1:%d/\n", port))
cat(sprintf("  Admin:   http://127.0.0.1:%d/?role=admin&key=%s\n\n", port, key))

shiny::runApp("app.R", host = "0.0.0.0", port = port, launch.browser = FALSE)
