# Player-facing module: join, submit a price each round, see your result.

player_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pg-narrow", shiny::uiOutput(ns("screen")))
}

player_server <- function(id, con) shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  token <- shiny::reactiveVal(
    shiny::parseQueryString(shiny::isolate(session$clientData$url_search))$t
  )

  state <- shiny::reactivePoll(1000, session,
    checkFunc = function() {
      s <- sl_active_session(con)
      if (is.null(s)) "none" else paste0(s$session_id, "@", sl_state_stamp(con, s$session_id))
    },
    valueFunc = function() {
      s <- sl_active_session(con)
      if (is.null(s)) NULL else sl_load_state(con, s$session_id)
    })

  me <- shiny::reactive({
    st <- state(); tk <- token()
    if (is.null(st) || is.null(tk) || !nzchar(tk)) return(NULL)
    p <- sl_player_by_token(con, tk)
    if (is.null(p) || p$session_id != st$session$session_id) return(NULL)
    p
  })

  my_submission <- shiny::reactive({
    st <- state(); p <- me(); if (is.null(st) || is.null(p)) return(NULL)
    s <- st$submissions[st$submissions$player_id == p$player_id, , drop = FALSE]
    if (nrow(s)) s else NULL
  })

  my_result <- shiny::reactive({
    st <- state(); p <- me(); if (is.null(st) || is.null(p)) return(NULL)
    r <- st$results[st$results$player_id == p$player_id &
                    st$results$round == st$session$current_round, , drop = FALSE]
    if (nrow(r)) r else NULL
  })

  # A coarse key for the student's screen, held in a reactiveVal so the screen
  # re-renders ONLY when this value truly changes. A plain reactive() would still
  # invalidate the renderUI on every poll tick (Shiny doesn't value-diff those),
  # rebuilding the price input and wiping whatever the student had typed when
  # *another* student submitted. It depends on this student's own state only.
  screen_key <- shiny::reactiveVal("init")
  shiny::observe({
    k <- pl_screen_key(state(), me(), my_submission(), my_result())
    if (!identical(k, shiny::isolate(screen_key()))) screen_key(k)
  })

  shiny::observeEvent(input$join, {
    st <- state(); shiny::req(st)
    r <- tryCatch(sl_add_player(con, st$session$session_id, input$name),
                  error = function(e) e)
    if (inherits(r, "error")) {
      shiny::showNotification(conditionMessage(r), type = "error"); return()
    }
    token(r$token)
    shiny::updateQueryString(paste0("?t=", r$token), mode = "replace")
  })

  shiny::observeEvent(input$submit, {
    st <- state(); p <- me(); shiny::req(st, p)
    r <- tryCatch(sl_submit(con, st$session$session_id, p$player_id,
                            st$session$current_round, input$price, input$rationale),
                  error = function(e) e)
    if (inherits(r, "error")) shiny::showNotification(conditionMessage(r), type = "error")
    else shiny::showNotification("Submitted ✓", type = "message", duration = 2)
  })

  output$screen <- shiny::renderUI({
    screen_key()                      # the only reactive dependency (see above)
    shiny::isolate(pl_screen(ns, state(), me(), my_submission(), my_result()))
  })

  # Independent of screen_key so the countdown never rebuilds the price input.
  output$timer <- shiny::renderUI({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return(NULL)
    countdown_tag(sl_time_left(st$session), size = "3.4rem",
                  note = "round closes at 0:00 — a price in the box is sent automatically")
  })

  # In the last few seconds, auto-send whatever valid price is in the box so a
  # student who's typed a number but not hit Submit isn't scored as a no-show.
  auto_done <- shiny::reactiveVal(-1L)
  shiny::observe({
    shiny::invalidateLater(1000)
    st <- state(); p <- me()
    if (is.null(st) || is.null(p)) return()
    s <- st$session
    if (s$phase != "open" || auto_done() == s$current_round) return()
    tl <- sl_time_left(s)
    if (is.na(tl) || tl > 5) return()                       # only the last ~5 s
    price <- suppressWarnings(as.numeric(shiny::isolate(input$price)))
    if (is.na(price) || price < 0) return()                 # nothing usable typed
    sub <- shiny::isolate(my_submission())
    if (!is.null(sub) && isTRUE(sub$price[1] == price)) {    # already sent this value
      auto_done(s$current_round); return()
    }
    ok <- tryCatch({
      sl_submit(con, s$session_id, p$player_id, s$current_round, price,
                shiny::isolate(input$rationale) %||% "")
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      auto_done(s$current_round)
      shiny::showNotification("Time's up — your price was submitted automatically.",
                              type = "message", duration = 4)
    }
  })
})

pl_screen <- function(ns, st, p, sub, res) {
    if (is.null(st))
      return(shiny::div(class = "card", shiny::div(class = "card-body",
        "No game is running yet. Keep this page open — it updates itself.")))

    if (is.null(p)) return(pl_join(ns, st))

    ph <- st$session$phase
    on_bench <- !is.null(p$market_id) && is.na(p$market_id) && ph != "lobby"
    if (on_bench) return(pl_card("You're sitting this one out",
      "There was an odd number of players, so you're on the bench for this game. Watch the projector — you're back next game."))

    switch(ph,
      lobby    = pl_lobby(p, st),
      open     = pl_round(ns, p, st, sub),
      scored   = pl_result(p, st, res),
      finished = pl_finished(p, st),
      pl_card("Standby", "…"))
}

# A coarse identity for the student's screen. Must change when THIS student's own
# view changes (phase, round, pairing, their own submission/result) and NOT when
# another student acts — otherwise re-rendering wipes an in-progress price entry.
pl_screen_key <- function(st, p, sub, res) {
  if (is.null(st)) return("no-game")
  if (is.null(p))  return(paste0("join|", st$session$phase))
  paste(st$session$phase, st$session$current_round,
        p$market_id %||% "NA", p$firm_label %||% "NA",
        if (is.null(sub)) "-" else paste(sub$price[1], sub$rationale[1], sub$submitted_at[1]),
        if (is.null(res)) "-" else res$computed_at[1],
        sep = "|")
}

# --- screens ---------------------------------------------------------------

pl_card <- function(title, body) shiny::div(class = "card",
  shiny::div(class = "card-header", title),
  shiny::div(class = "card-body", shiny::p(body)))

pl_join <- function(ns, st) shiny::div(class = "card",
  shiny::div(class = "card-header", sprintf("Pricing game — %s", st$game$label)),
  shiny::div(class = "card-body",
    shiny::p(sprintf("%d player(s) in the lobby.", nrow(st$players))),
    shiny::textInput(ns("name"), "Your name", placeholder = "e.g. Jordan"),
    shiny::actionButton(ns("join"), "Join", class = "btn btn-primary")))

pl_lobby <- function(p, st) shiny::div(class = "card",
  shiny::div(class = "card-header", "You're in"),
  shiny::div(class = "card-body",
    shiny::div(class = "pg-big", p$display_name),
    shiny::p(class = "pg-muted",
      sprintf("Waiting for the instructor to start. %d player(s) joined.", nrow(st$players)))))

pl_round <- function(ns, p, st, sub) {
  submitted <- !is.null(sub)
  last <- if (submitted) sub$price[1] else NA_real_
  is_shadow <- identical(p$firm_label, "Shadow")
  who <- if (is_shadow)
    "You're a shadow this game — you're scored one-on-one against one of the firms in your market."
    else sprintf("You are %s. Your rival is the other firm in your market.", p$firm_label)
  shiny::div(class = "card",
    shiny::div(class = "card-header",
      sprintf("Round %d of %d", st$session$current_round, st$session$n_rounds)),
    shiny::div(class = "card-body",
      shiny::div(style = "text-align:center;margin-bottom:.75rem;",
                 shiny::uiOutput(ns("timer"))),
      shiny::p(shiny::strong(who)),
      shiny::numericInput(ns("price"), "Your price ($)",
        value = if (is.na(last)) NULL else last, min = 0, step = 1),
      shiny::textAreaInput(ns("rationale"), "Why this price? (optional)",
        value = if (submitted) sub$rationale[1] else "", rows = 2),
      shiny::actionButton(ns("submit"),
        if (submitted) "Update price" else "Submit price", class = "btn btn-primary"),
      if (submitted) shiny::p(class = "pg-muted",
        sprintf("✓ Submitted %s. You can change it until the round closes.",
                money0(sub$price[1])))))
}

pl_result <- function(p, st, r) {
  rnd <- st$session$current_round
  if (is.null(r)) return(pl_card(sprintf("Round %d results", rnd),
    "Your market wasn't scored this round."))
  if (isTRUE(r$no_submission == 1))
    return(pl_card(sprintf("Round %d results", rnd),
      sprintf("You didn't submit a price, so you were out of the market — no sales, $0 this round. Cumulative profit: %s.",
              money0(r$cumulative_profit))))
  rival_txt <- if (is.na(r$rival_price)) "no submission" else money0(r$rival_price)
  shiny::div(class = "card",
    shiny::div(class = "card-header", sprintf("Round %d results", rnd)),
    shiny::div(class = "card-body",
      stat_block("Your price",   money0(r$price)),
      stat_block("Rival price",  rival_txt),
      stat_block("Units sold",   num1(r$quantity)),
      stat_block("Round profit", money0(r$profit)),
      shiny::p(class = "pg-muted", sprintf("Cumulative profit: %s", money0(r$cumulative_profit))),
      shiny::p("Waiting for the next round…")))
}

pl_finished <- function(p, st) {
  mine <- st$results[st$results$player_id == p$player_id, , drop = FALSE]
  mine <- mine[order(mine$round), ]
  tbl <- data.frame(
    Round        = mine$round,
    `Your price` = vapply(mine$price, money0, ""),
    `Rival price`= vapply(mine$rival_price, money0, ""),
    Profit       = vapply(mine$profit, money0, ""),
    Cumulative   = vapply(mine$cumulative_profit, money0, ""),
    check.names = FALSE)
  total <- if (nrow(mine)) max(mine$cumulative_profit) else 0
  shiny::div(class = "card",
    shiny::div(class = "card-header", "Game over"),
    shiny::div(class = "card-body",
      shiny::div(class = "pg-big", sprintf("Total profit: %s", money0(total))),
      html_table(tbl)))
}
