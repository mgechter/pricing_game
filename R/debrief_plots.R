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
  # wrap the strip to its panel's width, so three panels across don't clip it
  cols <- ggplot2::wrap_dims(length(strips), ncol = ncol)[2]
  wrap <- max(18, floor(96 / cols))

  ggplot(res, aes(round, price, colour = firm, group = firm)) +
    .benchmark_layers(d$benchmarks) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 1.8) +
    facet_wrap(~ market, ncol = ncol, labeller = label_wrap_gen(width = wrap)) +
    scale_colour_manual(values = FIRM_COLOURS) +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    expand_limits(y = 0) +
    labs(x = "Round", y = "Price ($)", colour = NULL,
         title = sprintf("%s: price paths", d$game$label %||% "Game")) +
    PG_THEME()
}

# The NE / monopoly (/ collusive) lines, each labelled with its value at the
# right edge. `extra` adds columns to the line data (a facet variable, say, so
# the lines land in one panel only).
.benchmark_layers <- function(b, extra = NULL) {
  ln <- .benchmark_lines(b)
  ln$text <- paste(ln$label, .usd(ln$y))
  if (!is.null(extra)) for (k in names(extra)) ln[[k]] <- extra[[k]]
  list(
    geom_hline(data = ln, aes(yintercept = y, linetype = label), colour = "grey45"),
    geom_text(data = ln, aes(x = Inf, y = y, label = text), inherit.aes = FALSE,
              hjust = 1.05, vjust = -0.45, size = 3, colour = "grey30"),
    scale_linetype_manual(values = c(NE = "dashed", Monopoly = "dotted",
                                     Collusive = "dotdash"), guide = "none"))
}

QUARTILE_COLOURS <- c(`75th percentile` = "#E69F00", Median = "black",
                      `25th percentile` = "#56B4E9")

# The spread of the class's prices each round: the 25th percentile, median and
# 75th percentile of every price scored that round.
plot_price_quartiles <- function(d) {
  res <- d$results[is.finite(d$results$price), , drop = FALSE]
  if (!nrow(res)) stop("no scored prices yet", call. = FALSE)
  rounds <- sort(unique(res$round))
  qs <- do.call(rbind, lapply(rounds, function(r) {
    p <- stats::quantile(res$price[res$round == r], c(0.75, 0.5, 0.25), names = FALSE)
    data.frame(round = r, stat = names(QUARTILE_COLOURS), price = p)
  }))
  qs$stat <- factor(qs$stat, levels = names(QUARTILE_COLOURS))

  ggplot(qs, aes(round, price, colour = stat, group = stat)) +
    .benchmark_layers(d$benchmarks) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = QUARTILE_COLOURS) +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    expand_limits(y = 0) +
    labs(x = "Round", y = "Price ($)", colour = NULL,
         title = d$game$label %||% "Game",
         subtitle = "The spread of prices by round: the 25th percentile, median and 75th percentile") +
    PG_THEME()
}

# One market's prices and round profits side by side, with its rank among the
# markets by the two firms' total profit.
plot_market_detail <- function(d, market_id, anon = FALSE) {
  res <- d$results[d$results$market_id == market_id, , drop = FALSE]
  if (!nrow(res)) stop("that market isn't available", call. = FALSE)
  measures <- c("Price ($)", "Round profit ($)")
  long <- rbind(
    data.frame(round = res$round, firm = res$firm_label, measure = measures[1], value = res$price),
    data.frame(round = res$round, firm = res$firm_label, measure = measures[2], value = res$profit))
  long$measure <- factor(long$measure, levels = measures)
  long$firm    <- factor(long$firm, levels = names(FIRM_COLOURS))
  floor0 <- data.frame(round = min(res$round), value = 0,
                       measure = factor(measures, levels = measures))
  rk <- dbf_market_ranks(d)
  rank_line <- if (market_id %in% rk$market_id)
    sprintf("Rank %d of %d markets by the two firms' total profit (%s)",
            rk$rank[rk$market_id == market_id], nrow(rk),
            .usd(rk$joint_profit[rk$market_id == market_id]))

  ggplot(long, aes(round, value, colour = firm, group = firm)) +
    .benchmark_layers(d$benchmarks,
                      list(measure = factor(measures[1], levels = measures))) +
    geom_blank(data = floor0, aes(round, value), inherit.aes = FALSE) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 1.8) +
    facet_wrap(~ measure, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = FIRM_COLOURS) +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    labs(x = "Round", y = NULL, colour = NULL,
         title = .market_strips(res, anon)[[1]], subtitle = rank_line) +
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

# Where a price sits against the game's benchmarks, in words.
.price_relation <- function(p, b, tol = 1e-6) {
  ne <- b$ne %||% NA_real_; mono <- b$monopoly %||% NA_real_
  if (is.finite(mono) && p > mono + tol) sprintf("above the monopoly price (%s)", .usd(mono))
  else if (is.finite(mono) && abs(p - mono) <= tol) sprintf("at the monopoly price (%s)", .usd(mono))
  else if (is.finite(ne) && p < ne - tol) sprintf("below the Nash price (%s)", .usd(ne))
  else if (is.finite(ne) && abs(p - ne) <= tol) sprintf("at the Nash price (%s)", .usd(ne))
  else sprintf("between the Nash (%s) and monopoly (%s) prices", .usd(ne), .usd(mono))
}

# One firm's best-response panel: its round profit at every own price, given
# the rival's actual price that round, and where its actual price, its best
# response, NE and monopoly sit.
.br_panel <- function(d, market_id, round, focal, anon = FALSE) {
  r  <- d$results[d$results$market_id == market_id & d$results$round == round, ]
  fr <- r[r$firm_label == focal, ]
  # the rival is the other firm; a shadow in an odd-headcount market is not one
  rr <- r[!(r$firm_label %in% c(focal, "Shadow")), ]
  if (nrow(fr) != 1 || nrow(rr) != 1 || is.na(rr$price))
    stop("that market / round isn't available", call. = FALSE)

  rival_p <- rr$price
  br <- best_response(d$model, d$params, rival_p)
  b  <- d$benchmarks
  marks <- data.frame(kind = c("Actual", "Best response", "NE", "Monopoly"),
                      x = c(fr$price, br$price, b$ne %||% NA, b$monopoly %||% NA))
  marks <- marks[is.finite(marks$x), , drop = FALSE]
  point <- if (is.finite(fr$price))
    data.frame(x = fr$price, y = own_profit(d$model, d$params, fr$price, rival_p))
    else data.frame(x = numeric(0), y = numeric(0))
  name <- function(row) if (!anon && nzchar(row$display_name %||% "")) row$display_name
                        else row$firm_label
  list(curve = br$curve, marks = marks, point = point,
       focal = name(fr), rival = name(rr), rival_p = rival_p,
       relation = .price_relation(rival_p, b))
}

MARK_COLOURS <- c(Actual = "forestgreen", `Best response` = "firebrick",
                  NE = "grey40", Monopoly = "#0072B2")
MARK_LINES   <- c(Actual = "solid", `Best response` = "dashed",
                  NE = "dashed", Monopoly = "dotted")

# Draw one or more .br_panel()s, stacked. Each line is labelled with its value
# beside it (staggered down the panel so lines at the same price stay legible);
# `strips` = one label per panel, or NULL for a single panel without a strip.
.plot_br_panels <- function(panels, strips = NULL) {
  key <- factor(if (is.null(strips)) rep("only", length(panels)) else strips,
                levels = unique(if (is.null(strips)) "only" else strips))
  stack <- function(part) do.call(rbind, Map(function(p, k) {
    x <- p[[part]]; if (nrow(x)) x$panel <- k else x$panel <- key[0]; x
  }, panels, as.list(key)))
  curve <- stack("curve"); marks <- stack("marks"); pts <- stack("point")

  # labels sit in a band above the curve's peak, one row per kind of line
  top  <- tapply(curve$profit, curve$panel, max)
  top[!is.finite(top) | top <= 0] <- 1
  lo   <- min(curve$price); hi <- max(curve$price)
  step <- c(Actual = 1.42, `Best response` = 1.31, NE = 1.20, Monopoly = 1.09)
  marks$ly   <- as.numeric(top[as.character(marks$panel)]) * step[marks$kind]
  marks$hj   <- ifelse(marks$x > lo + 0.72 * (hi - lo), 1.04, -0.04)
  marks$text <- sprintf("%s %s", marks$kind, .usd(marks$x))
  band <- data.frame(panel = factor(names(top), levels = levels(key)),
                     price = lo, profit = as.numeric(top) * 1.48)

  g <- ggplot(curve, aes(price, profit)) +
    geom_blank(data = band) +
    geom_line(linewidth = 1) +
    geom_vline(data = marks, aes(xintercept = x, colour = kind, linetype = kind),
               linewidth = 0.7) +
    geom_text(data = marks, aes(x = x, y = ly, label = text, colour = kind, hjust = hj),
              vjust = 0.5, size = 3.3, show.legend = FALSE) +
    geom_point(data = pts, aes(x = x, y = y), inherit.aes = FALSE,
               colour = MARK_COLOURS[["Actual"]], size = 2.6) +
    scale_colour_manual(values = MARK_COLOURS, guide = "none") +
    scale_linetype_manual(values = MARK_LINES, guide = "none") +
    labs(x = "Own price ($)", y = "Round profit ($)") +
    PG_THEME()
  if (!is.null(strips)) g <- g + facet_wrap(~ panel, ncol = 1, scales = "free_y")
  g
}

# Payoff of one firm as a function of its own price, given the rival's actual
# price in a chosen market/round; marks the actual choice, best response, NE
# and monopoly.
plot_best_response <- function(d, market_id, round, focal = "Firm A", anon = FALSE) {
  p <- .br_panel(d, market_id, round, focal, anon)
  .plot_br_panels(list(p)) +
    labs(title = sprintf("%s's round %d profit at each own price (market %s)",
                         p$focal, round, market_id),
         subtitle = sprintf("given %s's price of %s, %s", p$rival, .usd(p$rival_p),
                            p$relation))
}

# Both firms of one market in one round, stacked: each one's profit at every own
# price given the other's actual price. `first` is the firm drawn on top.
plot_best_response_pair <- function(d, market_id, round, first = "Firm A", anon = FALSE) {
  firms <- c(first, setdiff(c("Firm A", "Firm B"), first))
  ps <- lapply(firms, function(f) .br_panel(d, market_id, round, f, anon))
  strips <- vapply(ps, function(p) sprintf("%s, given %s's price of %s, %s",
                                           p$focal, p$rival, .usd(p$rival_p), p$relation), "")
  .plot_br_panels(ps, strips) +
    labs(title = sprintf("Market %s, round %d: each firm's profit at each own price",
                         market_id, round))
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
    save("price_quartiles",   plot_price_quartiles(d), w = 8, h = 4.5),
    save("cumulative_profit", plot_profit_paths(d))
  )
  # every market's prices and profits, named by its rank in joint profit
  rk <- dbf_market_ranks(d)
  for (i in seq_len(nrow(rk)))
    files <- c(files, save(sprintf("market_rank%d_m%s", rk$rank[i], rk$market_id[i]),
                           plot_market_detail(d, rk$market_id[i]), w = 11, h = 4.5))
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
