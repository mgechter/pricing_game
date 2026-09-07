# Pricing game — Shiny app.
#
#   Player view:      <url>/
#   Admin view:       <url>/?role=admin&key=<PRICING_ADMIN_KEY>
#   Projector view:   <url>/?mode=display
#
# Env vars (all optional):
#   PRICING_DB         sqlite file path        (default data/pricing_game.sqlite)
#   PRICING_ADMIN_KEY  admin key               (default "admin")
#   PRICING_GAMES      games.yaml path         (default config/games.yaml)

library(shiny)
library(bslib)

source("R/load_engine.R"); source_engine()
source("R/db.R")
source("R/pairing.R")
source("R/session_logic.R")
source("R/export.R")
source("R/debrief_data.R")
source("R/debrief_plots.R")
source("R/ui_helpers.R")
source("R/mod_player.R")
source("R/mod_admin.R")
source("R/mod_display.R")

DB_PATH    <- Sys.getenv("PRICING_DB",        "data/pricing_game.sqlite")
ADMIN_KEY  <- Sys.getenv("PRICING_ADMIN_KEY", "admin")
GAMES_PATH <- Sys.getenv("PRICING_GAMES",     "config/games.yaml")

dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
.con <- db_connect(DB_PATH)
db_init(.con)
db_assert_writable(.con)          # stop now if a stale server still holds the lock
message("database: ", normalizePath(DB_PATH))
onStop(function() try({ db_checkpoint(.con); DBI::dbDisconnect(.con) }, silent = TRUE))

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#7a0019"),
  tags$head(tags$style(HTML("
    .pg-narrow { max-width: 640px;  margin: 1.5rem auto; }
    .pg-wide   { max-width: 1100px; margin: 1.5rem auto; }
    .pg-big    { font-size: 1.9rem; font-weight: 600; margin-bottom: .5rem; }
    .pg-muted  { color: #6c757d; }
    .card      { margin-bottom: 1rem; }
  "))),
  uiOutput("root")
)

server <- function(input, output, session) {
  q    <- parseQueryString(isolate(session$clientData$url_search))
  mode <- if (identical(q$mode, "display")) "display"
          else if (identical(q$role, "admin")) "admin"
          else "player"
  ok_key <- identical(q$key, ADMIN_KEY)

  output$root <- renderUI({
    switch(mode,
      display = display_ui("display"),
      admin   = if (ok_key) admin_ui("admin")
                else div(class = "pg-narrow", class = "card", div(class = "card-body",
                  h4("Admin key required"),
                  p(class = "pg-muted", "Append ", tags$code("&key=…"), " to the URL."))),
      player  = player_ui("player"))
  })

  switch(mode,
    display = display_server("display", .con),
    admin   = if (ok_key) admin_server("admin", .con, GAMES_PATH),
    player  = player_server("player", .con))
}

shinyApp(ui, server)
