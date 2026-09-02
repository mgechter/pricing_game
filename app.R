# Pricing game — Shiny app (Phase 1 MVP).
#
#   Player view:  <url>/
#   Admin view:   <url>/?role=admin&key=<PRICING_ADMIN_KEY>
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
source("R/ui_helpers.R")
source("R/mod_player.R")
source("R/mod_admin.R")

DB_PATH    <- Sys.getenv("PRICING_DB",        "data/pricing_game.sqlite")
ADMIN_KEY  <- Sys.getenv("PRICING_ADMIN_KEY", "admin")
GAMES_PATH <- Sys.getenv("PRICING_GAMES",     "config/games.yaml")

dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
.con <- db_connect(DB_PATH)
db_init(.con)
onStop(function() try(DBI::dbDisconnect(.con), silent = TRUE))

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
  q        <- parseQueryString(isolate(session$clientData$url_search))
  is_admin <- identical(q$role, "admin")
  ok_key   <- identical(q$key, ADMIN_KEY)

  output$root <- renderUI({
    if (is_admin && !ok_key)
      div(class = "pg-narrow", class = "card",
          div(class = "card-body",
              h4("Admin key required"),
              p(class = "pg-muted", "Append ", tags$code("&key=…"), " to the URL.")))
    else if (is_admin) admin_ui("admin")
    else player_ui("player")
  })

  if (is_admin && ok_key) admin_server("admin", .con, GAMES_PATH)
  else if (!is_admin)     player_server("player", .con)
}

shinyApp(ui, server)
