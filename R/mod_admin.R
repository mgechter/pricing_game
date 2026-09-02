# Admin module: create a session, run the round loop, monitor, export.

admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pg-wide", shiny::uiOutput(ns("screen")))
}

admin_server <- function(id, con, games_path) shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  game_choices <- names(load_games(games_path))

  state <- shiny::reactivePoll(1000, session,
    checkFunc = function() {
      s <- sl_active_session(con)
      if (is.null(s)) "none" else paste0(s$session_id, "@", sl_state_stamp(con, s$session_id))
    },
    valueFunc = function() {
      s <- sl_active_session(con)
      if (is.null(s)) NULL else sl_load_state(con, s$session_id)
    })

  sid <- function() state()$session$session_id
  carry_rv <- shiny::reactiveVal(FALSE)   # persists across screen re-renders
  shiny::observeEvent(input$carry, carry_rv(isTRUE(input$carry)), ignoreInit = TRUE)

  guard <- function(expr) tryCatch(expr, error = function(e)
    shiny::showNotification(conditionMessage(e), type = "error"))

  shiny::observeEvent(input$create,
    guard(sl_create_session(con, input$game_id, games_path, label = input$new_label)))

  shiny::observeEvent(input$start, {
    st <- state(); shiny::req(st)
    guard({
      info <- sl_start_game(con, sid())
      if (length(info$sitting_out))
        shiny::showNotification("Odd number of players — one is sitting out.", type = "warning")
    })
  })

  shiny::observeEvent(input$score, {
    st <- state(); shiny::req(st)
    r <- tryCatch(sl_score_round(con, sid(), carry_forward = carry_rv()),
                  error = function(e) e)
    if (inherits(r, "error")) {
      shiny::showNotification(conditionMessage(r), type = "error")
    } else if (length(r$no_shows)) {
      shiny::showNotification(sprintf(
        "Scored %d market(s). No submission from %s — treated as out of the market ($0; rival gets the sole-seller outcome). Re-score after they submit if you like.",
        r$markets_scored, paste(r$no_shows, collapse = ", ")),
        type = "warning", duration = 9)
    } else {
      shiny::showNotification(sprintf("Scored %d market(s).", r$markets_scored), type = "message")
    }
  })

  shiny::observeEvent(input$advance, {
    st <- state(); shiny::req(st); guard(sl_advance_round(con, sid()))
  })

  shiny::observeEvent(input$reset, {
    st <- state(); shiny::req(st)
    shiny::showModal(shiny::modalDialog(
      title = "Reset this game to the lobby?",
      shiny::p("This permanently deletes every price and result recorded in this ",
               "session. The player roster is kept, so you can replay from round 1. ",
               "This cannot be undone."),
      shiny::p(class = "pg-muted",
               "To keep this session's data, use “New session” instead — it stays in History."),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
        shiny::actionButton(ns("reset_ok"), "Reset — delete this session's data",
                            class = "btn btn-danger"))))
  })
  shiny::observeEvent(input$reset_ok, {
    st <- state(); shiny::req(st)
    guard({ sl_reset_to_lobby(con, sid()); shiny::showNotification("Game reset to lobby.", type = "message") })
    shiny::removeModal()
  })

  shiny::observeEvent(input$newsession, sl_clear_active(con))

  output$dl <- shiny::downloadHandler(
    filename = function() sprintf("pricing_game_session_%s.csv", sid() %||% "x"),
    content  = function(file) {
      st <- state(); shiny::req(st)
      utils::write.csv(sl_export_frame(con, sid()), file, row.names = FALSE)
    })

  # ---- history: full CRUD over past sessions -------------------------------
  view_rv   <- shiny::reactiveVal("run")        # "run" | "history"
  hist_tick <- shiny::reactiveVal(0)            # bump to refresh the history list
  bump      <- function() hist_tick(hist_tick() + 1)
  del_target <- shiny::reactiveVal(NULL)
  ren_target <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$nav_run,     view_rv("run"))
  shiny::observeEvent(input$nav_history, view_rv("history"))
  shiny::observeEvent(input$create,     bump())
  shiny::observeEvent(input$newsession, bump())
  shiny::observeEvent(input$advance,    bump())

  # wire per-row buttons / download handlers for every session, once each
  bound <- shiny::reactiveVal(character(0))
  shiny::observe({
    hist_tick()
    sess <- sl_list_sessions(con)
    for (k in seq_len(nrow(sess))) {
      i <- as.character(sess$session_id[k])
      if (i %in% bound()) next
      local({
        sc <- i
        shiny::observeEvent(input[[paste0("open_", sc)]], {
          guard({ sl_set_active(con, as.integer(sc)); view_rv("run"); bump() })
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("ren_", sc)]], {
          cur <- sl_list_sessions(con)
          lbl <- cur$label[cur$session_id == as.integer(sc)]
          ren_target(as.integer(sc))
          shiny::showModal(shiny::modalDialog(title = paste("Rename session", sc),
            shiny::textInput(ns("ren_label"), "Label",
                             value = if (length(lbl) && !is.na(lbl)) lbl else ""),
            footer = shiny::tagList(shiny::modalButton("Cancel"),
              shiny::actionButton(ns("ren_ok"), "Save", class = "btn btn-primary"))))
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("del_", sc)]], {
          del_target(as.integer(sc))
          shiny::showModal(shiny::modalDialog(title = paste("Delete session", sc, "?"),
            shiny::p("This permanently removes the session and all of its players, submissions and results."),
            footer = shiny::tagList(shiny::modalButton("Cancel"),
              shiny::actionButton(ns("del_ok"), "Delete permanently", class = "btn btn-danger"))))
        }, ignoreInit = TRUE)
        output[[paste0("dl_", sc)]] <- shiny::downloadHandler(
          filename = function() sprintf("pricing_game_session_%s.csv", sc),
          content  = function(file)
            utils::write.csv(sl_export_frame(con, as.integer(sc)), file, row.names = FALSE))
      })
      bound(c(bound(), i))
    }
  })

  shiny::observeEvent(input$ren_ok, {
    guard(sl_set_label(con, ren_target(), input$ren_label))
    shiny::removeModal(); bump()
  })
  shiny::observeEvent(input$del_ok, {
    guard(sl_delete_session(con, del_target()))
    shiny::removeModal(); bump()
    shiny::showNotification("Session deleted.", type = "message")
  })

  output$screen <- shiny::renderUI({
    hist_tick()
    st <- state()
    nav <- ad_nav(ns, view_rv())
    if (identical(view_rv(), "history"))
      return(shiny::tagList(nav, ad_history(ns, sl_list_sessions(con))))

    body <- if (is.null(st)) ad_create(ns, game_choices)
      else switch(st$session$phase,
        lobby    = ad_lobby(ns, st, base_url(session)),
        open     = ad_round(ns, st, "open", carry_rv()),
        scored   = ad_round(ns, st, "scored", carry_rv()),
        finished = ad_finished(ns, st),
        ad_create(ns, game_choices))
    shiny::tagList(nav, body)
  })
})

# --- screens ---------------------------------------------------------------

ad_nav <- function(ns, view) {
  lk <- function(id, lbl, on) shiny::actionLink(ns(id), lbl,
    style = if (on) "font-weight:600; text-decoration:none;" else "text-decoration:none;")
  shiny::div(style = "margin-bottom:1rem;",
    lk("nav_run", "Run", view == "run"), shiny::HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"),
    lk("nav_history", "History", view == "history"))
}

ad_create <- function(ns, choices) shiny::div(class = "card",
  shiny::div(class = "card-header", "Start a new session"),
  shiny::div(class = "card-body",
    shiny::selectInput(ns("game_id"), "Game", choices = choices),
    shiny::textInput(ns("new_label"), "Label (optional)", placeholder = "e.g. Section A — Bertrand"),
    shiny::actionButton(ns("create"), "Create session", class = "btn btn-primary"),
    shiny::p(class = "pg-muted", style = "margin-top:.75rem;",
      "Sessions are saved automatically — see the History tab to revisit or export past ones.")))

ad_history <- function(ns, sess) {
  if (nrow(sess) == 0)
    return(shiny::div(class = "card", shiny::div(class = "card-body",
      "No sessions yet. Start one from the Run tab.")))
  row_ui <- function(s) {
    sc <- as.character(s$session_id)
    status <- if (s$phase %in% c("lobby", "finished")) s$phase
              else sprintf("%s · round %d/%d", s$phase, s$current_round, s$n_rounds)
    shiny::tags$tr(
      shiny::tags$td(sc, if (isTRUE(s$is_active))
        shiny::tags$span(class = "badge bg-primary", "active")),
      shiny::tags$td(if (is.na(s$label))
        shiny::tags$em(class = "pg-muted", "(unnamed)") else s$label),
      shiny::tags$td(s$game_id),
      shiny::tags$td(status),
      shiny::tags$td(s$n_players),
      shiny::tags$td(s$n_result_rows),
      shiny::tags$td(fmt_time(s$created_at)),
      shiny::tags$td(style = "white-space:nowrap;",
        shiny::actionButton(ns(paste0("open_", sc)), "Open",
                            class = "btn btn-sm btn-outline-primary"), " ",
        shiny::downloadButton(ns(paste0("dl_", sc)), "CSV",
                              class = "btn btn-sm btn-outline-secondary"), " ",
        shiny::actionButton(ns(paste0("ren_", sc)), "Rename", class = "btn btn-sm btn-link"), " ",
        shiny::actionButton(ns(paste0("del_", sc)), "Delete",
                            class = "btn btn-sm btn-link text-danger")))
  }
  shiny::div(class = "card",
    shiny::div(class = "card-header", "Session history"),
    shiny::div(class = "card-body",
      shiny::p(class = "pg-muted",
        "Every session is kept automatically. Delete the false starts you don't want to keep."),
      shiny::tags$table(class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(lapply(
          c("#", "Label", "Game", "Status", "Players", "Result rows", "Created", ""),
          shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(sess)), function(i) row_ui(sess[i, ]))))))
}

ad_lobby <- function(ns, st, url) {
  ready <- nrow(st$players) >= 2
  start_btn <- if (ready)
    shiny::actionButton(ns("start"), "Start game", class = "btn btn-primary")
  else
    shiny::tags$button("Start game — need 2+ players", class = "btn btn-primary", disabled = NA)
  shiny::div(class = "card",
    shiny::div(class = "card-header",
      sprintf("Session #%d — %s · lobby", st$session$session_id, st$game$label)),
    shiny::div(class = "card-body",
      shiny::p(shiny::strong("Student link: "), shiny::tags$code(url)),
      shiny::p(class = "pg-muted", "Students open that link, enter a name, and wait."),
      html_table(data.frame(Player = st$players$display_name,
                            Joined = fmt_time(st$players$joined_at))),
      start_btn, " ",
      shiny::actionButton(ns("newsession"), "Discard & choose another game",
                          class = "btn btn-link")))
}

ad_market_table <- function(st) {
  p <- st$players[!is.na(st$players$market_id), , drop = FALSE]
  if (nrow(p) == 0) return(NULL)
  p <- p[order(p$market_id, p$firm_label), ]
  sub <- st$submissions
  res <- st$results[st$results$round == st$session$current_round, , drop = FALSE]
  price_of <- function(id) {
    row <- sub[sub$player_id == id, , drop = FALSE]
    if (nrow(row))
      return(paste0(money0(row$price[1]),
                    if (isTRUE(row$carried[1] == 1)) " (carried)" else ""))
    rr <- res[res$player_id == id, , drop = FALSE]
    if (nrow(rr) && isTRUE(rr$no_submission[1] == 1)) return("(no submission)")
    "—"
  }
  profit_of <- function(id) { v <- res$profit[res$player_id == id]
    if (length(v)) money0(v) else "" }
  data.frame(
    Market = p$market_id,
    Firm   = p$firm_label,
    Player = p$display_name,
    Price  = vapply(p$player_id, price_of, ""),
    Profit = vapply(p$player_id, profit_of, ""),
    check.names = FALSE)
}

ad_round <- function(ns, st, phase, carry = FALSE) {
  s <- st$session; rs <- st$round_status
  bench <- st$players$display_name[is.na(st$players$market_id)]
  controls <- if (phase == "open")
    shiny::tagList(
      shiny::checkboxInput(ns("carry"),
        "Carry forward missing prices from last round (otherwise a no-show is scored out of the market)",
        value = carry),
      shiny::actionButton(ns("score"), "Close & score round", class = "btn btn-primary"))
  else
    shiny::tagList(
      shiny::actionButton(ns("advance"),
        if (s$current_round >= s$n_rounds) "Finish game" else "Next round",
        class = "btn btn-primary"), " ",
      shiny::actionButton(ns("score"), "Re-score", class = "btn btn-outline-secondary"))

  shiny::div(class = "card",
    shiny::div(class = "card-header",
      sprintf("Session #%d — %s", s$session_id, st$game$label)),
    shiny::div(class = "card-body",
      shiny::div(class = "pg-big",
        sprintf("Round %d of %d — %s", s$current_round, s$n_rounds, phase)),
      shiny::p(sprintf("%d of %d submitted.%s", rs$n_submitted, rs$n_expected,
        if (length(rs$missing)) paste0("  Waiting on: ", paste(rs$missing, collapse = ", ")) else "")),
      html_table(ad_market_table(st)),
      if (length(bench)) shiny::p(class = "pg-muted",
        paste("Sitting out:", paste(bench, collapse = ", "))),
      shiny::hr(),
      controls,
      shiny::span(style = "margin-left:1rem;", shiny::downloadButton(ns("dl"), "Export CSV")),
      shiny::hr(),
      shiny::actionButton(ns("reset"), "Reset game for a do-over…",
                          class = "btn btn-link text-danger")))
}

ad_finished <- function(ns, st) {
  res <- st$results
  tot <- stats::aggregate(profit ~ display_name, res, sum)
  tot <- tot[order(-tot$profit), ]
  lb  <- data.frame(Player = tot$display_name,
                    `Total profit` = vapply(tot$profit, money0, ""),
                    check.names = FALSE)
  shiny::div(class = "card",
    shiny::div(class = "card-header",
      sprintf("Session #%d — %s · finished", st$session$session_id, st$game$label)),
    shiny::div(class = "card-body",
      shiny::h5("Leaderboard"), html_table(lb),
      shiny::downloadButton(ns("dl"), "Export CSV"), " ",
      shiny::actionButton(ns("newsession"), "New session", class = "btn btn-primary"),
      shiny::p(class = "pg-muted", style = "margin-top:.75rem;",
        "This session is saved in History. Start a fresh one with “New session”.")))
}
