# Projector / display module — read-only big-screen view for the room.
#   URL:  <host>:3838/?mode=display
#
# What it shows is set per session by `projector_view`:
#   summary     average price per round + leaderboard   (default)
#   scoreboard  leaderboard only
#   detail      per-market price paths + leaderboard     (busy with many teams)
#
# The chart and leaderboard update ONLY when a round is scored — not as students
# submit. The submission counter and the countdown stay live.

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

  # Reload the debrief data only when the *scored* picture changes (round scored
  # or re-scored, phase change, anon / view toggled) — not on every submission.
  # The latest scoring time is in the key because a re-score ("carrying the
  # no-shows' prices forward") changes profits without changing the row count.
  d_key <- shiny::reactiveVal("")
  shiny::observe({
    st <- state()
    scored_at <- if (!is.null(st) && nrow(st$results))
      max(st$results$computed_at %||% 0, na.rm = TRUE) else 0
    k <- if (is.null(st)) "none" else paste(
      st$session$session_id, st$session$phase, st$session$current_round,
      nrow(st$results), format(scored_at, digits = 15), st$session$anonymous %||% 0,
      st$session$projector_view %||% "summary")
    if (!identical(k, shiny::isolate(d_key()))) d_key(k)
  })
  d <- shiny::reactive({
    d_key()
    st <- shiny::isolate(state())
    if (is.null(st) || !nrow(st$results)) return(NULL)
    dd <- tryCatch(dbf_load(con, st$session$session_id), error = function(e) NULL)
    if (!is.null(dd)) {
      dd$anon <- isTRUE((st$session$anonymous %||% 0) == 1)
      dd$view <- st$session$projector_view %||% "summary"
    }
    dd
  })

  output$timer <- shiny::renderUI({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return(NULL)
    countdown_tag(sl_time_left(st$session), size = "5rem")
  })
  output$chart <- shiny::renderPlot({
    dd <- shiny::req(d())
    if (dd$view == "detail") plot_price_paths(dd, anon = TRUE) else plot_avg_price(dd)
  }, height = 480)
  output$lb <- shiny::renderTable({
    dd <- shiny::req(d())
    lb <- dbf_leaderboards(dd)$individuals
    data.frame(
      Rank = seq_len(nrow(lb)),
      Firm = if (dd$anon) sprintf("Market %s · %s", lb$market_id, lb$firm_label)
             else lb$display_name,
      Profit = vapply(lb$total_profit, money2, ""),
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
      shiny::h2(sprintf("%d of %d submitted", rs$n_confirmed, rs$n_expected)),
      if (!have_results)
        shiny::h2("Results appear here once round 1 is scored.")
      else shiny::tagList(
        if ((s$projector_view %||% "summary") != "scoreboard")
          shiny::plotOutput(ns("chart"), height = "480px"),
        shiny::h2("Leaderboard"),
        shiny::tableOutput(ns("lb"))))
  })
})
