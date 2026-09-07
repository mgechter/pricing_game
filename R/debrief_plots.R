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

plot_price_paths <- function(d, anon = FALSE) {
  res <- d$results
  res$firm <- if (anon) res$firm_label else res$display_name
  res$market <- paste("Market", res$market_id)
  ln <- .benchmark_lines(d$benchmarks)

  ggplot(res, aes(round, price, colour = firm, group = firm)) +
    geom_hline(data = ln, aes(yintercept = y, linetype = label), colour = "grey45") +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 1.8) +
    facet_wrap(~ market) +
    scale_x_continuous(breaks = function(l) seq(1, ceiling(max(l)), by = 1)) +
    labs(x = "Round", y = "Price ($)", colour = NULL, linetype = "Benchmark",
         title = sprintf("%s — price paths", d$game$label %||% "Game")) +
    PG_THEME()
}

plot_avg_price <- function(d) {
  sub <- d$submissions[!is.na(d$submissions$price), ]
  ap  <- aggregate(price ~ round, sub, mean)
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
  rr <- r[r$firm_label != focal, ]
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
                            formatC(rival_p, format = "f", digits = 0))) +
    PG_THEME()
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
  for (i in seq_len(nrow(flags))) {
    nm <- sprintf("best_response_m%s_r%s", flags$market_id[i], flags$round[i])
    files <- c(files, tryCatch(
      save(nm, plot_best_response(d, flags$market_id[i], flags$round[i], flags$firm_label[i]),
           w = 8, h = 4.5),
      error = function(e) character(0)))
  }
  files
}
