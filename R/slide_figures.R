# ECON 30303: the figures the course's debrief slides show after the three
# games, drawn from the latest scored session of each game, and the two kinds
# the 2024 debrief showed. The instructor's "Slide figures" tab shows them in
# slide order, each with a PNG download, and a ZIP holds them all with a
# captions file. Class-only, not for upstream.

# One entry per figure, in the order the slides use them. `kind` picks the
# figure; `id` names its PNG by what it shows. The first seven are the current
# debrief deck's; `set = "2024"` marks the kind the 2024 debrief showed (2024's
# other kind, one market's prices and profits, has its own picker and goes
# into the ZIP's markets/ folder for every market).
SLIDE_FIGURES <- list(
  list(id = "bertrand_price_paths",                      game = "bertrand",       kind = "paths"),
  list(id = "bertrand_rival_above_monopoly",             game = "bertrand",       kind = "above_monopoly"),
  list(id = "hotelling_high_price_paths",                game = "hotelling_high", kind = "paths"),
  list(id = "hotelling_high_rival_below_ne",             game = "hotelling_high", kind = "below_ne"),
  list(id = "hotelling_low_price_paths",                 game = "hotelling_low",  kind = "paths"),
  list(id = "hotelling_low_rival_above_monopoly_both_firms", game = "hotelling_low", kind = "above_monopoly_pair"),
  list(id = "hotelling_low_last_round_cuts",             game = "hotelling_low",  kind = "last_round_cuts"),
  list(id = "bertrand_price_quartiles",       game = "bertrand",       kind = "quartiles", set = "2024"),
  list(id = "hotelling_high_price_quartiles", game = "hotelling_high", kind = "quartiles", set = "2024"),
  list(id = "hotelling_low_price_quartiles",  game = "hotelling_low",  kind = "quartiles", set = "2024")
)

.sf_game_name <- function(d) {
  if (identical(d$model, "bertrand_homogeneous")) "Bertrand"
  else sprintf("Differentiated products, t = %s", format(d$params$t))
}

# What a rival-price case is, in words, without the students' names.
.sf_case_note <- function(cs, d, kind) {
  b <- d$benchmarks
  where <- .price_relation(cs$rival_price, b)
  if (cs$exact)
    return(sprintf("Market %s, round %d: the rival priced %s, %s.",
                   cs$market_id, cs$round, .usd(cs$rival_price), where))
  line <- if (kind == "above_monopoly")
    sprintf("No rival priced above the monopoly price (%s) in this game", .usd(b$monopoly))
    else sprintf("No rival priced below the Nash price (%s) in this game", .usd(b$ne))
  sprintf("%s, so this is the nearest case: market %s, round %d, a rival price of %s, %s.",
          line, cs$market_id, cs$round, .usd(cs$rival_price), where)
}

# Build one figure: list(id, title, source, note, plot, w, h); plot is NULL
# when the game has no session yet or the figure can't be drawn.
slide_figure_one <- function(f, d, anon = FALSE) {
  x <- list(id = f$id, set = f$set %||% "current", title = f$id, source = NULL,
            note = NULL, plot = NULL, w = 10, h = 5)
  if (is.null(d)) {
    x$note <- sprintf("No scored session of the %s game yet.", f$game)
    return(x)
  }
  g <- .sf_game_name(d)
  last <- max(d$results$round)
  lab  <- d$session$label
  named <- length(lab) == 1 && !is.na(lab) && nzchar(trimws(lab))   # a session made without a label has NA
  x$source <- sprintf("Session #%s%s: %d round%s scored%s.",
                      d$session$session_id, if (named) paste0(", ", lab) else "", last,
                      if (last == 1) "" else "s",
                      if (!identical(d$session$phase, "finished")) ", game still running" else "")
  res <- tryCatch(switch(f$kind,
    paths = {
      n <- length(unique(d$results$market_id))
      list(title = sprintf("%s: every market's price path", g),
           plot = plot_price_paths(d, anon, ncol = 2) + labs(title = sprintf("%s: price paths", g)),
           w = 10, h = 1.6 + 2.3 * ceiling(n / 2))
    },
    above_monopoly = , below_ne = {
      cs <- dbf_rival_case(d, f$kind)
      if (is.null(cs)) stop("no firm-round with a rival's price", call. = FALSE)
      list(title = sprintf("%s: a firm whose rival priced %s", g,
                           if (f$kind == "below_ne") "below the Nash price" else "above the monopoly price"),
           plot = plot_best_response(d, cs$market_id, cs$round, cs$firm_label, anon),
           note = .sf_case_note(cs, d, f$kind),
           w = 9, h = if (f$kind == "below_ne") 5.5 else 4.2)
    },
    above_monopoly_pair = {
      cs <- dbf_rival_case(d, "above_monopoly")
      if (is.null(cs)) stop("no firm-round with a rival's price", call. = FALSE)
      list(title = sprintf("%s: both firms of a market where one priced above the monopoly price", g),
           plot = plot_best_response_pair(d, cs$market_id, cs$round, first = cs$firm_label, anon),
           note = paste(.sf_case_note(cs, d, "above_monopoly"),
                        "The firm facing that price is on top."),
           w = 10, h = 6.5)
    },
    last_round_cuts = {
      lc <- dbf_last_round_cuts(d, 2)
      if (!nrow(lc)) stop("the game has one round, so no last-round cut to show", call. = FALSE)
      cuts <- ifelse(lc$cut > 0, sprintf("%s in market %s", .usd(lc$cut), lc$market_id),
                     sprintf("none in market %s", lc$market_id))
      note <- sprintf("In round %d, the last round played, the biggest price cut%s %s.", last,
                      if (nrow(lc) > 1) "s were" else " was", paste(cuts, collapse = " and "))
      if (!all(lc$exact))
        note <- paste(note, "Fewer than two markets had both firms above the Nash price",
                      "before cutting in the last round, so this includes the next-biggest cuts.")
      list(title = sprintf("%s: the two biggest last-round price cuts", g),
           plot = plot_price_paths(d, anon, markets = lc$market_id, ncol = 1) +
             labs(title = sprintf("%s: last-round price cuts", g)),
           note = note, w = 6.5, h = 1.6 + 2.6 * nrow(lc))
    },
    quartiles = list(title = sprintf("%s: the spread of prices by round", g),
                     plot = plot_price_quartiles(d) + labs(title = g), w = 8, h = 4.5)),
    error = function(e) list(title = sprintf("%s: %s", g, f$id),
                             note = sprintf("Can't draw this figure: %s.", conditionMessage(e))))
  x[names(res)] <- res
  x
}

# The latest scored session of each game, loaded (NULL for a game not played).
slide_figures_data <- function(con) {
  ss <- sl_list_sessions(con)
  ss <- ss[ss$n_result_rows > 0, , drop = FALSE]
  games <- unique(vapply(SLIDE_FIGURES, `[[`, "", "game"))
  lapply(stats::setNames(games, games), function(g) {
    s <- ss[ss$game_id == g, , drop = FALSE]
    if (!nrow(s)) NULL else dbf_load(con, max(s$session_id))
  })
}

slide_figures_from <- function(ds, anon = FALSE) {
  out <- lapply(SLIDE_FIGURES, function(f) slide_figure_one(f, ds[[f$game]], anon))
  stats::setNames(out, vapply(SLIDE_FIGURES, `[[`, "", "id"))
}

# Every figure, from the latest scored session of each game.
slide_figures_build <- function(con, anon = FALSE)
  slide_figures_from(slide_figures_data(con), anon)

# The one-market picker's choices, grouped by game, markets by rank in joint
# profit; values are "<game>|<market id>".
slide_market_choices <- function(ds) {
  ch <- list()
  for (g in names(ds)) {
    d <- ds[[g]]
    if (is.null(d)) next
    rk <- dbf_market_ranks(d)
    if (!nrow(rk)) next
    ch[[.sf_game_name(d)]] <- stats::setNames(
      paste(g, rk$market_id, sep = "|"),
      sprintf("Market %s (rank %d of %d by joint profit)", rk$market_id, rk$rank, nrow(rk)))
  }
  ch
}

# One market's prices and profits, from a "<game>|<market id>" value.
slide_market_figure <- function(ds, value, anon = FALSE) {
  parts <- strsplit(value %||% "", "|", fixed = TRUE)[[1]]
  if (length(parts) != 2 || is.null(ds[[parts[1]]])) return(NULL)
  d  <- ds[[parts[1]]]
  mk <- suppressWarnings(as.integer(parts[2]))
  if (!(mk %in% d$results$market_id)) return(NULL)
  rk <- dbf_market_ranks(d)
  list(id = sprintf("%s_rank%d_market%d", parts[1], rk$rank[rk$market_id == mk], mk),
       title = sprintf("%s, market %d", .sf_game_name(d), mk),
       plot = plot_market_detail(d, mk, anon) +
         labs(title = paste0(.sf_game_name(d), ". ", .market_strips(
           d$results[d$results$market_id == mk, ], anon)[[1]])),
       w = 10.5, h = 4.3)
}

slide_figure_save <- function(x, file, dpi = 200) {
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
  ggplot2::ggsave(file, x$plot, device = dev, width = x$w, height = x$h, dpi = dpi)
  file
}

# The figures that could be drawn, as <id>.png in `dir`, plus captions.txt
# saying where each came from; with `ds` (slide_figures_data()), also every
# market's prices and profits in markets/, named <game>_rank<k>_market<id>.png.
slide_figures_write <- function(figs, dir, ds = NULL, anon = FALSE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- character(0)
  cap <- character(0)
  for (x in figs) {
    cap <- c(cap, paste0(x$id, ".png"), paste0("  ", x$title),
             if (!is.null(x$source)) paste0("  ", x$source),
             if (!is.null(x$note)) paste0("  ", x$note), "")
    if (!is.null(x$plot))
      files <- c(files, slide_figure_save(x, file.path(dir, paste0(x$id, ".png"))))
  }
  values <- unlist(slide_market_choices(ds %||% list()), use.names = FALSE)
  if (length(values)) {
    dir.create(file.path(dir, "markets"), showWarnings = FALSE)
    cap <- c(cap, "markets/", "  One market's prices and round profits, for every market of every game,",
             "  named <game>_rank<k>_market<id>.png by its rank in the two firms' total profit.", "")
    for (v in values) {
      x <- slide_market_figure(ds, v, anon)
      files <- c(files, slide_figure_save(x, file.path(dir, "markets", paste0(x$id, ".png"))))
    }
  }
  writeLines(cap, file.path(dir, "captions.txt"))
  c(files, file.path(dir, "captions.txt"))
}

# --- the tab ---------------------------------------------------------------

# `figs` carries the one-market picker's choices as attr "markets"; `picked`
# is the market already picked, kept across a redraw.
ad_slide_figures <- function(ns, figs, anon = FALSE, picked = NULL) {
  if (is.null(figs)) return(shiny::div(class = "card", shiny::div(class = "card-body", "Drawing…")))
  head <- shiny::div(class = "card", shiny::div(class = "card-body",
    shiny::p("The debrief slides' figures, in slide order, each drawn from the latest",
             "scored session of its game. Refresh after a game ends."),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput(ns("sf_anon"), "Show as Firm A / B", anon)),
      shiny::column(8, shiny::div(style = "text-align:right;",
        shiny::actionButton(ns("sf_refresh"), "Refresh", class = "btn btn-sm btn-outline-secondary"), " ",
        shiny::downloadButton(ns("sf_zip"), "Download all (ZIP)", class = "btn btn-sm btn-outline-primary"))))))
  card <- function(x) shiny::div(class = "card",
    shiny::div(class = "card-header", x$title),
    shiny::div(class = "card-body",
      shiny::p(class = "pg-muted", style = "margin-bottom:.3rem;",
               paste0(x$id, ".png", if (!is.null(x$source)) paste0(". ", x$source) else "")),
      if (!is.null(x$note)) shiny::p(x$note),
      if (!is.null(x$plot)) shiny::tagList(
        shiny::plotOutput(ns(paste0("sf_plot_", x$id)), width = "auto", height = "auto"),
        shiny::downloadButton(ns(paste0("sf_dl_", x$id)), "PNG",
                              class = "btn btn-sm btn-outline-secondary"))))
  is24 <- vapply(figs, function(x) identical(x$set, "2024"), logical(1))
  heading <- function(txt) shiny::h5(txt, style = "margin:1.5rem 0 .8rem;")

  choices <- attr(figs, "markets") %||% list()
  values  <- unlist(choices, use.names = FALSE)
  market_card <- shiny::div(class = "card",
    shiny::div(class = "card-header", "One market's prices and profits"),
    shiny::div(class = "card-body",
      if (!length(values)) shiny::p("No scored session yet.")
      else shiny::tagList(
        shiny::p(class = "pg-muted", style = "margin-bottom:.3rem;",
                 "The ZIP's markets/ folder has this figure for every market of every game."),
        shiny::selectInput(ns("sf_market"), "Market", choices = choices,
                           selected = if (length(intersect(picked, values))) picked else values[1]),
        shiny::plotOutput(ns("sf_market_plot"), width = "auto", height = "auto"),
        shiny::downloadButton(ns("sf_market_dl"), "PNG",
                              class = "btn btn-sm btn-outline-secondary"))))

  shiny::tagList(head,
    heading("The current debrief slides"), lapply(figs[!is24], card),
    heading("Like the 2024 debrief: the spread of prices by round, and one market's prices and profits"),
    lapply(figs[is24], card), market_card)
}

# Server half; call inside admin_server's moduleServer. Returns the figures
# reactive for the tab's renderUI.
slide_figures_server <- function(input, output, con, view_rv) {
  # drawn afresh each time the tab is opened or Refresh is clicked, and not at
  # all while another tab is showing
  tick <- shiny::reactiveVal(0)
  shiny::observeEvent(input$sf_refresh, tick(tick() + 1))
  data <- shiny::reactive({
    tick()
    if (!identical(view_rv(), "figures")) return(NULL)
    slide_figures_data(con)
  })
  figs <- shiny::reactive({
    ds <- data(); if (is.null(ds)) return(NULL)
    out <- slide_figures_from(ds, anon = isTRUE(input$sf_anon))
    attr(out, "markets") <- slide_market_choices(ds)
    out
  })
  market_fig <- shiny::reactive({
    ds <- shiny::req(data())
    slide_market_figure(ds, input$sf_market %||% unlist(slide_market_choices(ds))[1],
                        anon = isTRUE(input$sf_anon))
  })
  output$sf_market_plot <- shiny::renderPlot({
    x <- shiny::req(market_fig()); x$plot
  }, width = function() round(100 * (market_fig()$w %||% 1)),
     height = function() round(100 * (market_fig()$h %||% 1)), res = 100)
  output$sf_market_dl <- shiny::downloadHandler(
    filename = function() paste0(shiny::req(market_fig())$id, ".png"),
    content  = function(file) slide_figure_save(shiny::req(market_fig()), file))
  for (f in SLIDE_FIGURES) local({
    id <- f$id
    # on screen at 100 pixels to the inch, so it has the PNG's own layout
    px <- function(dim) function() {
      x <- figs()[[id]]
      if (is.null(x) || is.null(x$plot)) 100 else round(100 * x[[dim]])
    }
    output[[paste0("sf_plot_", id)]] <- shiny::renderPlot({
      x <- shiny::req(figs())[[id]]; shiny::req(x$plot); x$plot
    }, width = px("w"), height = px("h"), res = 100)
    output[[paste0("sf_dl_", id)]] <- shiny::downloadHandler(
      filename = function() paste0(id, ".png"),
      content  = function(file) slide_figure_save(shiny::req(figs())[[id]], file))
  })
  output$sf_zip <- shiny::downloadHandler(
    filename = function() "slide_figures.zip",
    content  = function(file) {
      dir <- tempfile("slide_figures_")
      files <- slide_figures_write(shiny::req(figs()), dir, ds = data(),
                                   anon = isTRUE(input$sf_anon))
      zip::zip(file, files = substring(files, nchar(dir) + 2), root = dir)
    })
  figs
}
