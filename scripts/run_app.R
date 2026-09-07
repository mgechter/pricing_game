#!/usr/bin/env Rscript
# Launch the pricing game locally.  From the project root:
#
#   Rscript scripts/run_app.R                       # port 3838
#   PRICING_ADMIN_KEY=secret Rscript scripts/run_app.R
#   PRICING_DEV=1 Rscript scripts/run_app.R         # hot-reload on file save (dev only)
#
# Then expose it to the class for the hour with a tunnel, e.g.
#   ngrok http 3838
#   tailscale funnel 3838
#
# Player link:  http://<host>:3838/
# Admin link:   http://<host>:3838/?role=admin&key=<PRICING_ADMIN_KEY>   (default key: "admin")

port <- as.integer(Sys.getenv("PRICING_PORT", "3838"))
key  <- Sys.getenv("PRICING_ADMIN_KEY", "admin")
dev  <- tolower(Sys.getenv("PRICING_DEV", "")) %in% c("1", "true", "yes", "on")

if (dev) {
  options(
    shiny.autoreload = TRUE,         # re-source the app + refresh browsers on save
    shiny.autoreload.pattern = "\\.(r|html?|js|css|ya?ml)$")
  message("PRICING_DEV: hot-reload ON — every connected browser refreshes on save. ",
          "Do NOT use during a real class. (games.yaml changes only affect NEW sessions.)")
}

cat(sprintf("\n  Player:  http://127.0.0.1:%d/\n", port))
cat(sprintf("  Admin:   http://127.0.0.1:%d/?role=admin&key=%s\n\n", port, key))

shiny::runApp("app.R", host = "0.0.0.0", port = port, launch.browser = FALSE)
