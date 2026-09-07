# Projector / display module — read-only big-screen view for the room.
#   URL:  <host>:3838/?mode=display
#
# What it shows is set per session by `projector_view`:
#   summary     average price per round + leaderboard   (default)
#   scoreboard  leaderboard only
#   detail      per-market price paths + leaderboard     (busy with many teams)

display_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pg-wide", style = "max-width:1400px;",
    shiny::tags$style(shiny::HTML(
      ".pg-disp h1{font-size:2.6rem;margin:.2rem 0} .pg-disp h2{font-size:1.8rem;color:#495057}
       .pg-disp table{font-size:1.3rem}")),
    shiny::div(class = "pg-disp", shiny::uiOutput(ns("screen"))))
}

display_server <- function(id, con) shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  state <- shiny::reactivePoll(1000, session,
    checkFunc = function() {
      s <- sl_active_session(con)
      if (is.null(s)) "none" else paste0(s$session_id, "@", sl_state_stamp(con, s$session_id))
    },
    valueFunc = function() {
      s <- sl_active_session(con)
      if (is.null(s)) NULL else sl_load_state(con, s$session_id)
    })

  anon <- function() isTRUE(state()$session$anonymous == 1)
  view <- function() state()$session$projector_view %||% "summary"

  d <- shiny::reactive({
    st <- state()
    if (is.null(st) || !nrow(st$results)) return(NULL)
    tryCatch(dbf_load(con, st$session$session_id), error = function(e) NULL)
  })

  output$timer <- shiny::renderUI({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return(NULL)
    countdown_tag(sl_time_left(st$session), size = "5rem")
  })
  output$chart <- shiny::renderPlot({
    shiny::req(d())
    if (view() == "detail") plot_price_paths(d(), anon = TRUE) else plot_avg_price(d())
  }, height = 480)
  output$lb <- shiny::renderTable({
    shiny::req(d())
    lb <- dbf_leaderboards(d())$individuals
    data.frame(
      Rank = seq_len(nrow(lb)),
      Firm = if (anon()) sprintf("Market %s · %s", lb$market_id, lb$firm_label) else lb$display_name,
      Profit = vapply(lb$total_profit, money0, ""),
      check.names = FALSE)
  }, striped = TRUE, width = "100%")

  output$screen <- shiny::renderUI({
    st <- state()
    if (is.null(st)) return(shiny::h1("Waiting for the game to start…"))
    s <- st$session; rs <- st$round_status
    have_results <- nrow(st$results) > 0
    shiny::tagList(
      shiny::div(style = "display:flex;justify-content:space-between;align-items:center;gap:2rem;",
        shiny::h1(st$game$label),
        shiny::uiOutput(ns("timer"))),
      shiny::h2(sprintf("Round %d of %d — %s", s$current_round, s$n_rounds, s$phase)),
      shiny::h2(sprintf("%d of %d submitted", rs$n_submitted, rs$n_expected)),
      if (!have_results)
        shiny::h2("Results appear here once round 1 is scored.")
      else shiny::tagList(
        if (view() != "scoreboard") shiny::plotOutput(ns("chart"), height = "480px"),
        shiny::h2("Leaderboard"),
        shiny::tableOutput(ns("lb"))))
  })
})
