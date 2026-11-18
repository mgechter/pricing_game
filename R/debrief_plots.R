# Debrief charts. Pure ggplot builders — take a `d` list from dbf_load() /
# dbf_load_bundle() and return a ggplot object. Used by the Shiny debrief tab and
# by debrief.qmd.

suppressPackageStartupMessages(library(ggplot2))

PG_THEME <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.position = "bottom")
}

.benchmark_lines <- function(b) {
  ln <- data.frame(y = c(b$ne, b$monopoly), label = c("NE", "Monopoly"))
  if (!is.null(b$collusive) && is.finite(b$collusive) &&
      abs(b$collusive - b$monopoly) > 1e-6)
    ln <- rbind(ln, data.frame(y = b$collusive, label = "Collusive"))
  ln[is.finite(ln$y), , drop = FALSE]
}

# Dollar amounts print to the cent, as everywhere else in the app.
.usd <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))

FIRM_COLOURS <- c(`Firm A` = "#D55E00", `Firm B` = "#0072B2", Shadow = "#CC79A7")

# One strip label per market, naming who is Firm A and who is Firm B (and the
# shadow, if the market has one), so a panel reads as a pair without a legend
# of every player in the game.
.market_strips <- function(res, anon = FALSE) {
  mk <- sort(unique(res$market_id))
  lab <- vapply(mk, function(m) {
    if (anon) return(sprintf("Market %s", m))
    r  <- res[res$market_id == m, ]
    nm <- function(f) {
      v <- unique(r$display_name[r$firm_label == f])
      if (length(v)) v[1] else NA_character_
    }
    out <- sprintf("Market %s: %s (A) vs %s (B)", m, nm("Firm A"), nm("Firm B"))
    if (!is.na(nm("Shadow"))) out <- sprintf("%s, shadow %s", out, nm("Shadow"))
    out
  }, character(1))
  stats::setNames(lab, mk)
}

# `markets`: market ids to draw (NULL = all). `ncol`: facet columns (NULL lets
# ggplot choose).
plot_price_paths <- function(d, anon = FALSE, markets = NULL, ncol = NULL) {
  res <- d$results
  if (length(markets)) res <- res[res$market_id %in% markets, , drop = FALSE]
  if (!nrow(res)) stop("none of those markets has results", call. = FALSE)
  strips <- .market_strips(res, anon)
  res$market <- factor(strips[as.character(res$market_id)], levels = strips)
  res$firm   <- factor(res$firm_label, levels = names(FIRM_COLOURS))
  ln <- .benchmark_lines(d$benchmarks)
  ln$text <- paste(ln$label, .usd(ln$y))
  # wrap the strip to its panel's width, so three panels across don't clip it
  cols <- ggplot2::wrap_dims(length(strips), ncol = ncol)[2]
  wrap <- max(18, floor(96 / cols))

  ggplot(res, aes(round, price, colour = firm, group = firm)) +
    geom_hline(data = ln, aes(yintercept = y, linetype = label), colour = "grey45") +
    geom_text(data = ln, aes(x = Inf, y = y, label = text), inherit.aes = FALSE,
              hjust = 1.05, vjust = -0.45, size = 3, colour = "grey30") +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 1.8) +
    facet_wrap(~ market, ncol = ncol, labeller = label_wrap_gen(width = wrap)) +
    scale_colour_manual(values = FIRM_COLOURS) +
    scale_linetype_manual(values = c(NE = "dashed", Monopoly = "dotted",
                                     Collusive = "dotdash"), guide = "none") +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    expand_limits(y = 0) +
    labs(x = "Round", y = "Price ($)", colour = NULL,
         title = sprintf("%s: price paths", d$game$label %||% "Game")) +
    PG_THEME()
}

plot_avg_price <- function(d) {
  # from results (scored rounds only) so it never shows a half-formed round
  res <- d$results[!is.na(d$results$price), ]
  ap  <- aggregate(price ~ round, res, mean)
  ln  <- .benchmark_lines(d$benchmarks)

  ggplot(ap, aes(round, price)) +
    geom_hline(data = ln, aes(yintercept = y, linetype = label),
               colour = "grey45") +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    labs(x = "Round", y = "Mean submitted price ($)", linetype = "Benchmark",
         title = "Average price per round") +
    PG_THEME()
}

plot_profit_paths <- function(d, anon = FALSE, cumulative = TRUE) {
  res <- d$results
  res$firm <- if (anon) res$firm_label else res$display_name
  res$market <- paste("Market", res$market_id)
  res$y <- if (cumulative) res$cumulative_profit else res$profit
  ggplot(res, aes(round, y, colour = firm, group = firm)) +
    geom_line(na.rm = TRUE) + geom_point(na.rm = TRUE, size = 1.8) +
    facet_wrap(~ market) +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    labs(x = "Round", y = if (cumulative) "Cumulative profit ($)" else "Round profit ($)",
         colour = NULL,
         title = sprintf("%s — %s profit", d$game$label %||% "Game",
                         if (cumulative) "cumulative" else "round")) +
    PG_THEME()
}

# Payoff of one firm as a function of its own price, given the rival's actual
# price in a chosen market/round; marks the actual choice, best response and NE.
plot_best_response <- function(d, market_id, round, focal = "Firm A") {
  r  <- d$results[d$results$market_id == market_id & d$results$round == round, ]
  fr <- r[r$firm_label == focal, ]
  # the rival is the other firm; a shadow in an odd-headcount market is not one
  rr <- r[!(r$firm_label %in% c(focal, "Shadow")), ]
  if (!nrow(fr) || !nrow(rr) || is.na(rr$price))
    stop("that market / round isn't available", call. = FALSE)

  rival_p <- rr$price
  br      <- best_response(d$model, d$params, rival_p)
  b       <- d$benchmarks
  vl <- data.frame(
    x = c(fr$price, br$price, b$ne),
    label = factor(c("Actual", "Best response", "NE"),
                   levels = c("Actual", "Best response", "NE")))
  vl <- vl[is.finite(vl$x), , drop = FALSE]

  fname <- if (nzchar(fr$display_name %||% "")) fr$display_name else focal
  rname <- if (nzchar(rr$display_name %||% "")) rr$display_name else rr$firm_label

  ggplot(br$curve, aes(price, profit)) +
    geom_line(linewidth = 1) +
    geom_vline(data = vl, aes(xintercept = x, colour = label), linetype = "dashed") +
    scale_colour_manual(values = c(Actual = "forestgreen",
                                   `Best response` = "firebrick", NE = "grey40")) +
    labs(x = "Own price ($)", y = "Round profit ($)", colour = NULL,
         title = sprintf("%s's payoff vs its own price — Market %s, round %d",
                         fname, market_id, round),
         subtitle = sprintf("given %s priced $%s", rname,
                            formatC(rival_p, format = "f", digits = 2))) +
    PG_THEME()
}

# One prediction question. own_r1 -> predicted-vs-actual scatter; choice -> bar;
# other numeric -> histogram with theory / realised-outcome lines.
plot_prediction <- function(item) {
  q  <- item$question
  aa <- item$answers
  ttl <- if (nchar(q$prompt) > 70) paste0(substr(q$prompt, 1, 67), "…") else q$prompt

  if (!is.null(item$per_student)) {
    ps <- item$per_student[is.finite(item$per_student$predicted) &
                           is.finite(item$per_student$actual), ]
    return(ggplot(ps, aes(predicted, actual)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
      geom_point(size = 2, colour = "#4c78a8") +
      labs(x = "predicted", y = "what you actually did", title = ttl) + PG_THEME())
  }

  if (identical(q$type, "choice") || all(is.na(aa$answer_num))) {
    tab <- as.data.frame(table(answer = aa$answer), stringsAsFactors = FALSE)
    return(ggplot(tab, aes(stats::reorder(answer, -Freq), Freq)) +
      geom_col(fill = "#4c78a8") +
      labs(x = NULL, y = "students", title = ttl) + PG_THEME())
  }

  ln <- data.frame(x = c(item$actual, q$reference),
                   label = factor(c("Actual", "Theory"), levels = c("Actual", "Theory")))
  ln <- ln[is.finite(ln$x), , drop = FALSE]
  p <- ggplot(aa[is.finite(aa$answer_num), ], aes(answer_num)) +
    geom_histogram(bins = 15, fill = "#4c78a8", colour = "white")
  if (nrow(ln))
    p <- p + geom_vline(data = ln, aes(xintercept = x, colour = label), linewidth = 1) +
      scale_colour_manual(values = c(Actual = "forestgreen", Theory = "grey30"))
  p + labs(x = "predicted value", y = "students", colour = NULL, title = ttl) + PG_THEME()
}

plot_predictions_grid <- function(items) {
  items <- Filter(function(it) !identical(it$question$type, "text"), items)
  if (!length(items)) return(NULL)
  plots <- lapply(items, plot_prediction)
  if (requireNamespace("patchwork", quietly = TRUE))
    patchwork::wrap_plots(plots, ncol = 2)
  else plots[[1]]
}

# The standard figure set for a session, written as PNGs into `dir`.
debrief_write_figs <- function(d, dir, width = 9, height = 5.5, dpi = 200) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
  save <- function(name, plot, w = width, h = height) {
    f <- file.path(dir, paste0(name, ".png"))
    ggsave(f, plot, device = dev, width = w, height = h, dpi = dpi)
    f
  }
  files <- c(
    save("price_paths",       plot_price_paths(d)),
    save("price_paths_anon",  plot_price_paths(d, anon = TRUE)),
    save("average_price",     plot_avg_price(d), w = 8, h = 4.5),
    save("cumulative_profit", plot_profit_paths(d))
  )
  flags <- dbf_flags(d)
  flags <- flags[flags$firm_label %in% c("Firm A", "Firm B"), , drop = FALSE]
  # one PNG per (market, round, firm): two flags on the same firm-round draw the
  # same figure, and a name without the firm let Firm A's and Firm B's overwrite
  flags <- flags[!duplicated(flags[c("market_id", "round", "firm_label")]), , drop = FALSE]
  for (i in seq_len(nrow(flags))) {
    nm <- sprintf("best_response_m%s_r%s_%s", flags$market_id[i], flags$round[i],
                  sub("^Firm ", "", flags$firm_label[i]))
    files <- c(files, tryCatch(
      save(nm, plot_best_response(d, flags$market_id[i], flags$round[i], flags$firm_label[i]),
           w = 8, h = 4.5),
      error = function(e) character(0)))
  }
  files
}
