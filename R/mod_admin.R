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
      if (!is.null(input$new_timer))
        sl_set_timer(con, sid(), input$new_timer, auto_advance = isTRUE(input$new_auto))
      if (!is.null(input$new_anon))
        sl_set_anonymous(con, sid(), isTRUE(input$new_anon))
      info <- sl_start_game(con, sid())
      if (!is.null(info$shadow)) {
        who <- db_one(con, "SELECT display_name FROM players WHERE player_id = ?",
                      list(info$shadow$player_id))$display_name
        shiny::showNotification(sprintf("Odd number — %s is playing as a shadow (scored 1-on-1 against a paired student).", who),
                                type = "message", duration = 6)
      }
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

  # export bundle for the sessions ticked in History
  bundle_ids <- function() as.integer(shiny::req(input$hist_pick))
  output$dl_bundle_zip <- shiny::downloadHandler(
    filename = function() sprintf("pricing_game_bundle_%s.zip", Sys.Date()),
    content  = function(file) write_bundle_zip(sl_export_bundle(con, bundle_ids()), file))
  output$dl_bundle_xlsx <- shiny::downloadHandler(
    filename = function() sprintf("pricing_game_bundle_%s.xlsx", Sys.Date()),
    content  = function(file) write_bundle_xlsx(sl_export_bundle(con, bundle_ids()), file))

  # ---- debrief ----------------------------------------------------------------
  shiny::observeEvent(input$nav_debrief, view_rv("debrief"))
  debrief_sid <- shiny::reactiveVal(NULL)
  dbf_tick    <- shiny::reactiveVal(0)

  scored_sessions <- function() {
    s <- sl_list_sessions(con)
    s[s$n_result_rows > 0, , drop = FALSE]
  }
  shiny::observeEvent(view_rv(), {
    if (view_rv() != "debrief") return()
    if (is.null(debrief_sid())) {
      s <- scored_sessions()
      if (nrow(s)) debrief_sid(s$session_id[1])
    }
  })
  shiny::observeEvent(input$debrief_pick, debrief_sid(as.integer(input$debrief_pick)))
  shiny::observeEvent(input$debrief_refresh, dbf_tick(dbf_tick() + 1))

  dbrief <- shiny::reactive({
    dbf_tick(); sid <- debrief_sid(); if (is.null(sid)) return(NULL)
    tryCatch(dbf_load(con, sid), error = function(e) NULL)
  })
  flags <- shiny::reactive({ d <- dbrief(); if (is.null(d)) NULL else dbf_flags(d) })

  # keep the best-response picker in range for the chosen session
  br_market <- shiny::reactive({
    d <- dbrief(); shiny::req(d)
    as.integer(input$br_market %||% min(d$results$market_id))
  })

  shiny::observeEvent(input$flag_pick, {
    fr <- flags(); shiny::req(fr, input$flag_pick != "")
    row <- fr[as.integer(input$flag_pick), ]
    if (row$firm_label %in% c("Firm A", "Firm B")) {
      shiny::updateSelectInput(session, "br_market", selected = row$market_id)
      shiny::updateSelectInput(session, "br_round",  selected = row$round)
      shiny::updateRadioButtons(session, "br_firm",  selected = row$firm_label)
    }
  })

  gg_png <- function(expr, w = 9, h = 5.5) shiny::downloadHandler(
    filename = function() "figure.png",
    content  = function(file) {
      dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
      ggplot2::ggsave(file, expr(), device = dev, width = w, height = h, dpi = 200)
    })

  output$db_price   <- shiny::renderPlot(plot_price_paths(shiny::req(dbrief()), isTRUE(input$db_anon)))
  output$db_avg     <- shiny::renderPlot(plot_avg_price(shiny::req(dbrief())))
  output$db_profit  <- shiny::renderPlot(plot_profit_paths(shiny::req(dbrief()), isTRUE(input$db_anon)))
  output$db_br <- shiny::renderPlot({
    d <- shiny::req(dbrief())
    plot_best_response(d, br_market(),
                       as.integer(input$br_round %||% min(d$results$round)),
                       input$br_firm %||% "Firm A")
  })
  output$dl_price  <- gg_png(function() plot_price_paths(dbrief(), isTRUE(input$db_anon)))
  output$dl_avg    <- gg_png(function() plot_avg_price(dbrief()), h = 4.5)
  output$dl_profit <- gg_png(function() plot_profit_paths(dbrief(), isTRUE(input$db_anon)))
  output$dl_br     <- gg_png(function() plot_best_response(dbrief(), br_market(),
                        as.integer(input$br_round), input$br_firm %||% "Firm A"), h = 4.5)

  output$db_flags   <- shiny::renderTable(
    { fr <- flags(); if (is.null(fr) || !nrow(fr)) return(NULL)
      data.frame(Flag = fr$kind, Market = fr$market_id, Round = fr$round, What = fr$detail) },
    striped = TRUE, width = "100%")
  output$db_lb_indiv <- shiny::renderTable({
    d <- shiny::req(dbrief()); lb <- dbf_leaderboards(d)$individuals
    data.frame(Player = lb$display_name, Market = lb$market_id, Firm = lb$firm_label,
               `Total profit` = vapply(lb$total_profit, money0, ""),
               `vs. all-NE`   = vapply(lb$vs_ne, function(x) sprintf("%s%s", if (x >= 0) "+" else "-", money0(abs(x))), ""),
               check.names = FALSE)
  }, striped = TRUE, width = "100%")
  output$db_lb_mkt <- shiny::renderTable({
    d <- shiny::req(dbrief()); lb <- dbf_leaderboards(d)$markets
    data.frame(Market = lb$market_id, `Firm A` = lb$`Firm A`, `Firm B` = lb$`Firm B`,
               `Market profit` = vapply(lb$market_profit, money0, ""), check.names = FALSE)
  }, striped = TRUE, width = "100%")

  output$dl_figpack <- shiny::downloadHandler(
    filename = function() sprintf("pricing_game_figs_session_%s.zip", debrief_sid() %||% "x"),
    content  = function(file) {
      d <- shiny::req(dbrief())
      dir <- tempfile("figs_"); files <- debrief_write_figs(d, dir)
      zip::zip(file, files = basename(files), root = dir)
    })

  # ---- round timer -----------------------------------------------------------
  output$round_timer <- shiny::renderText({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return("")
    tl <- sl_time_left(st$session)
    if (is.na(tl)) "" else if (tl <= 0) "⏱  Time's up."
    else sprintf("⏱  %d:%02d left in the round", tl %/% 60, floor(tl) %% 60)
  })
  shiny::observeEvent(list(input$timer_now, input$auto_now), {
    st <- state(); shiny::req(st, st$session$phase %in% c("open", "scored"))
    guard(sl_set_timer(con, sid(), input$timer_now %||% 0, auto_advance = isTRUE(input$auto_now)))
  }, ignoreInit = TRUE)

  last_auto <- shiny::reactiveVal(-1L)
  shiny::observe({
    shiny::invalidateLater(2000)
    st <- shiny::isolate(state()); if (is.null(st)) return()
    if (sl_should_auto_advance(st$session) && last_auto() != st$session$current_round) {
      last_auto(st$session$current_round)
      guard({ sl_score_round(con, sid(), carry_forward = carry_rv()); sl_advance_round(con, sid()) })
      shiny::showNotification("Timer expired — round scored and advanced.", type = "message")
    }
  })

  output$screen <- shiny::renderUI({
    hist_tick()
    st <- state()
    nav <- ad_nav(ns, view_rv())
    if (identical(view_rv(), "history"))
      return(shiny::tagList(nav, ad_history(ns, sl_list_sessions(con))))
    if (identical(view_rv(), "debrief"))
      return(shiny::tagList(nav, ad_debrief(ns, dbrief(), scored_sessions(),
                                            debrief_sid(), flags())))

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
  sep <- shiny::HTML("&nbsp;&nbsp;|&nbsp;&nbsp;")
  shiny::div(style = "margin-bottom:1rem;",
    lk("nav_run", "Run", view == "run"), sep,
    lk("nav_history", "History", view == "history"), sep,
    lk("nav_debrief", "Debrief", view == "debrief"))
}

ad_create <- function(ns, choices) shiny::div(class = "card",
  shiny::div(class = "card-header", "Start a new session"),
  shiny::div(class = "card-body",
    shiny::selectInput(ns("game_id"), "Game", choices = choices),
    shiny::textInput(ns("new_label"), "Label (optional)", placeholder = "e.g. Section A — Bertrand"),
    shiny::fluidRow(
      shiny::column(4, shiny::numericInput(ns("new_timer"),
        "Round timer (seconds, 0 = off)", value = 0, min = 0, step = 15)),
      shiny::column(4, shiny::div(style = "margin-top:1.9rem;",
        shiny::checkboxInput(ns("new_auto"), "Auto-score & advance when it expires", FALSE))),
      shiny::column(4, shiny::div(style = "margin-top:1.9rem;",
        shiny::checkboxInput(ns("new_anon"), "Hide names on projector / debrief", FALSE)))),
    shiny::actionButton(ns("create"), "Create session", class = "btn btn-primary"),
    shiny::p(class = "pg-muted", style = "margin-top:.75rem;",
      "Sessions are saved automatically — see the History tab to revisit or export past ones. ",
      "Projector view: add ", shiny::tags$code("?mode=display"), " to the URL.")))

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
  scored <- sess[sess$n_result_rows > 0, , drop = FALSE]
  choices <- stats::setNames(scored$session_id,
    sprintf("#%d — %s (%s)", scored$session_id,
            ifelse(is.na(scored$label), "unnamed", scored$label), scored$game_id))

  shiny::div(class = "card",
    shiny::div(class = "card-header", "Session history"),
    shiny::div(class = "card-body",
      shiny::p(class = "pg-muted",
        "Every session is kept automatically. Delete the false starts you don't want to keep."),
      shiny::tags$table(class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(lapply(
          c("#", "Label", "Game", "Status", "Players", "Result rows", "Created", ""),
          shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(sess)), function(i) row_ui(sess[i, ])))),
      if (nrow(scored)) shiny::tagList(
        shiny::hr(),
        shiny::h6("Download a class bundle"),
        shiny::p(class = "pg-muted",
          "Tick the sessions from one class, then download a tidy CSV/Excel bundle for analysis."),
        shiny::checkboxGroupInput(ns("hist_pick"), NULL, choices = choices, inline = TRUE),
        shiny::downloadButton(ns("dl_bundle_zip"),  "Download bundle (ZIP)",
                              class = "btn btn-sm btn-outline-primary"), " ",
        shiny::downloadButton(ns("dl_bundle_xlsx"), "Download bundle (Excel)",
                              class = "btn btn-sm btn-outline-primary"))))
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
  shad <- st$players[!is.na(st$players$shadow_of), , drop = FALSE]
  shadow_note <- if (nrow(shad)) {
    hosts <- st$players$display_name[match(shad$shadow_of, st$players$player_id)]
    shiny::p(class = "pg-muted",
      sprintf("Shadow: %s (scored 1-on-1 against %s)",
              paste(shad$display_name, collapse = ", "), paste(hosts, collapse = ", ")))
  }
  timer_row <- shiny::fluidRow(
    shiny::column(4, shiny::numericInput(ns("timer_now"), "Round timer (s)",
      value = s$round_seconds %||% 0, min = 0, step = 15)),
    shiny::column(4, shiny::div(style = "margin-top:1.9rem;",
      shiny::checkboxInput(ns("auto_now"), "Auto-score & advance on expiry",
                           value = isTRUE((s$auto_advance %||% 0) == 1)))),
    shiny::column(4, shiny::div(style = "margin-top:2rem;font-weight:600;",
      shiny::textOutput(ns("round_timer")))))
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
      timer_row,
      html_table(ad_market_table(st)),
      shadow_note,
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
        "This session is saved in History. Open the Debrief tab for charts, or start a fresh game with “New session”.")))
}

# --- debrief -------------------------------------------------------------------

ad_debrief <- function(ns, d, sessions, sid, fr) {
  if (nrow(sessions) == 0)
    return(shiny::div(class = "card", shiny::div(class = "card-body",
      "No finished games to debrief yet. Run one from the Run tab.")))

  choices <- stats::setNames(sessions$session_id,
    sprintf("#%d — %s (%s)", sessions$session_id,
            ifelse(is.na(sessions$label), "unnamed", sessions$label), sessions$game_id))
  picker <- shiny::div(class = "card", shiny::div(class = "card-body",
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(ns("debrief_pick"), "Session",
        choices = choices, selected = sid)),
      shiny::column(3, shiny::checkboxInput(ns("db_anon"),
        "Show as Firm A / B",
        value = !is.null(d) && isTRUE((d$session$anonymous %||% 0) == 1))),
      shiny::column(3, shiny::div(style = "margin-top:1.6rem;",
        shiny::actionButton(ns("debrief_refresh"), "Refresh", class = "btn btn-sm btn-outline-secondary"), " ",
        shiny::downloadButton(ns("dl_figpack"), "Figure pack (ZIP)", class = "btn btn-sm btn-outline-primary"))))))

  if (is.null(d))
    return(shiny::tagList(picker, shiny::div(class = "card",
      shiny::div(class = "card-body", "Loading…"))))

  fig_card <- function(title, plot_id, dl_id, height = 420) shiny::div(class = "card",
    shiny::div(class = "card-header", title),
    shiny::div(class = "card-body",
      shiny::plotOutput(ns(plot_id), height = paste0(height, "px")),
      shiny::downloadButton(ns(dl_id), "PNG", class = "btn btn-sm btn-outline-secondary")))

  rounds  <- sort(unique(d$results$round))
  markets <- sort(unique(d$results$market_id))
  flag_choices <- c("—" = "")
  if (!is.null(fr) && nrow(fr))
    flag_choices <- c(flag_choices, stats::setNames(as.character(seq_len(nrow(fr))),
      sprintf("Market %s, round %s — %s", fr$market_id, fr$round, fr$kind)))

  br_card <- shiny::div(class = "card",
    shiny::div(class = "card-header", "Best-response explorer"),
    shiny::div(class = "card-body",
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("br_market"), "Market", choices = markets)),
        shiny::column(3, shiny::selectInput(ns("br_round"), "Round", choices = rounds,
                                            selected = max(rounds))),
        shiny::column(3, shiny::radioButtons(ns("br_firm"), "Firm",
                                             choices = c("Firm A", "Firm B"), inline = TRUE)),
        shiny::column(3, shiny::selectInput(ns("flag_pick"), "Jump to a flag",
                                            choices = flag_choices))),
      shiny::plotOutput(ns("db_br"), height = "380px"),
      shiny::downloadButton(ns("dl_br"), "PNG", class = "btn btn-sm btn-outline-secondary")))

  shiny::tagList(
    picker,
    shiny::div(class = "card",
      shiny::div(class = "card-header", "Teachable moments"),
      shiny::div(class = "card-body", shiny::tableOutput(ns("db_flags")))),
    fig_card("Price paths", "db_price", "dl_price", 460),
    fig_card("Average price per round", "db_avg", "dl_avg", 360),
    br_card,
    fig_card("Cumulative profit", "db_profit", "dl_profit", 460),
    shiny::div(class = "card",
      shiny::div(class = "card-header", "Leaderboard — players"),
      shiny::div(class = "card-body", shiny::tableOutput(ns("db_lb_indiv")))),
    shiny::div(class = "card",
      shiny::div(class = "card-header", "Leaderboard — markets"),
      shiny::div(class = "card-body", shiny::tableOutput(ns("db_lb_mkt")))))
}
