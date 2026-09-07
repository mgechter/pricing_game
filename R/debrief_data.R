# Turn a session (or an exported bundle) into tidy frames + auto-flagged
# "teachable moments". Shared by the Shiny debrief tab and debrief.qmd, so the
# numbers on screen and in the report can't diverge.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# --- load ------------------------------------------------------------------

dbf_load <- function(con, session_id) {
  s <- sl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  game <- resolve_benchmarks(sl_game(s))

  players <- db_query(con,
    "SELECT player_id, display_name, market_id, firm_label FROM players
     WHERE session_id = ? AND market_id IS NOT NULL
     ORDER BY market_id, firm_label", list(session_id))

  results <- db_query(con,
    "SELECT r.round, r.market_id, r.player_id, p.display_name, p.firm_label,
            p.shadow_of, r.price, r.rival_price, r.share, r.quantity, r.revenue,
            r.profit, r.cumulative_profit, r.no_submission
     FROM results r JOIN players p ON p.player_id = r.player_id
     WHERE r.session_id = ? ORDER BY r.market_id, r.player_id, r.round",
    list(session_id))

  submissions <- db_query(con,
    "SELECT s.round, s.player_id, p.display_name, p.market_id, p.firm_label,
            s.price, s.rationale, s.carried
     FROM submissions s JOIN players p ON p.player_id = s.player_id
     WHERE s.session_id = ? ORDER BY s.round, p.market_id, p.firm_label",
    list(session_id))

  list(session = s, game = game,
       model = game$demand$model, params = game$demand$params,
       benchmarks = game$benchmarks,
       players = players, results = results, submissions = submissions)
}

# Same shape, from an unzipped export bundle (for offline use in debrief.qmd).
dbf_load_bundle <- function(dir, session_id = NULL) {
  rd <- function(f) utils::read.csv(file.path(dir, f), stringsAsFactors = FALSE)
  bm  <- rd("benchmarks.csv")
  if (is.null(session_id)) session_id <- bm$session_id[1]
  bm  <- bm[bm$session_id == session_id, ][1, ]
  res <- rd("results_long.csv"); res <- res[res$session_id == session_id, ]
  sub <- rd("submissions.csv");  sub <- sub[sub$session_id == session_id, ]
  params <- list(V = bm$V, t = bm$t, L = bm$L, M = bm$M, mc = bm$mc,
                 choke = bm$choke, coverage_decay = isTRUE(as.logical(bm$coverage_decay)))
  params <- params[!vapply(params, function(x) length(x) == 0 || is.na(x), logical(1))]
  list(
    session = list(session_id = session_id, n_rounds = bm$rounds, phase = bm$phase),
    game = list(label = bm$game_label, demand = list(model = bm$model, params = params)),
    model = bm$model, params = params,
    benchmarks = list(ne = bm$ne, collusive = bm$collusive, monopoly = bm$monopoly),
    players = unique(res[c("market_id", "firm_label", "display_name")]),
    results = res, submissions = sub)
}

# --- teachable moments ---------------------------------------------------------

.flags_cols <- c("kind", "market_id", "round", "firm_label", "who", "detail", "score")
.flags_empty <- function() {
  d <- as.data.frame(matrix(nrow = 0, ncol = length(.flags_cols)))
  names(d) <- .flags_cols; d
}

# longest run of TRUE in a logical vector -> list(len, start, end) (1-indexed)
.longest_true_run <- function(x) {
  x[is.na(x)] <- FALSE
  if (!any(x)) return(list(len = 0L, start = NA_integer_, end = NA_integer_))
  r <- rle(x)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  i <- which(r$values)[which.max(r$lengths[r$values])]
  list(len = r$lengths[i], start = starts[i], end = ends[i])
}

dbf_flags <- function(d, top_n = 2L) {
  res <- d$results
  ne  <- d$benchmarks$ne %||% 0
  if (!nrow(res)) return(.flags_empty())
  m0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  out <- list()

  swing <- res |>
    arrange(player_id, round) |>
    group_by(player_id) |>
    mutate(dp = profit - lag(profit)) |>
    ungroup() |>
    filter(!is.na(dp)) |>
    mutate(score = abs(dp))
  if (nrow(swing)) {
    swing <- slice_max(swing, score, n = top_n, with_ties = FALSE)
    out$swing <- data.frame(kind = "Profit swing", market_id = swing$market_id,
      round = swing$round, firm_label = swing$firm_label, who = swing$display_name,
      detail = sprintf("%s's round profit %s $%s vs the round before",
                       swing$display_name,
                       ifelse(swing$dp >= 0, "jumped", "fell"), m0(abs(swing$dp))),
      score = swing$score)
  }

  brs <- res |> filter(!is.na(rival_price), no_submission == 0)
  if (nrow(brs)) {
    brs$br <- vapply(brs$rival_price,
      function(pr) best_response(d$model, d$params, pr)$price, numeric(1))
    brs$score <- abs(brs$price - brs$br)
    brs <- slice_max(brs, score, n = top_n, with_ties = FALSE)
    out$br <- data.frame(kind = "Off best response", market_id = brs$market_id,
      round = brs$round, firm_label = brs$firm_label, who = brs$display_name,
      detail = sprintf("%s priced $%s; best response to the rival's $%s was $%s",
                       brs$display_name, m0(brs$price), m0(brs$rival_price), m0(brs$br)),
      score = brs$score)
  }

  wide <- res |> select(market_id, round, firm_label, price) |>
    pivot_wider(names_from = firm_label, values_from = price) |> arrange(market_id, round)
  for (mk in unique(wide$market_id)) {
    w <- wide[wide$market_id == mk, ]
    if (nrow(w) < 3 || !all(c("Firm A", "Firm B") %in% names(w))) next
    a <- w[["Firm A"]]; b <- w[["Firm B"]]
    col <- .longest_true_run(a > ne & b > ne)
    if (col$len >= 3)
      out[[paste0("col", mk)]] <- data.frame(kind = "Tacit collusion", market_id = mk,
        round = w$round[col$end], firm_label = "both", who = "both firms",
        detail = sprintf("both firms priced above NE ($%s) for rounds %d–%d",
                         m0(ne), w$round[col$start], w$round[col$end]),
        score = col$len)
    cas <- .longest_true_run(diff(a) < 0 & diff(b) < 0)
    if (cas$len >= 3)
      out[[paste0("cas", mk)]] <- data.frame(kind = "Undercut cascade", market_id = mk,
        round = w$round[cas$end + 1], firm_label = "both", who = "both firms",
        detail = sprintf("both firms cut price %d rounds running (through round %d)",
                         cas$len, w$round[cas$end + 1]),
        score = cas$len)
  }

  res_df <- if (length(out)) do.call(rbind, out) else .flags_empty()
  res_df[order(-res_df$score), , drop = FALSE]
}

# --- leaderboards -----------------------------------------------------------

dbf_leaderboards <- function(d) {
  res <- d$results
  nr  <- d$session$n_rounds %||% max(res$round, 0)
  ne  <- d$benchmarks$ne %||% 0
  ne_pr <- own_profit(d$model, d$params, ne, ne)          # symmetric per-round NE profit

  indiv <- res |>
    group_by(market_id, firm_label, display_name) |>
    summarise(total_profit = sum(profit), .groups = "drop") |>
    mutate(vs_ne = total_profit - nr * ne_pr) |>
    arrange(desc(total_profit))

  markets <- res |>
    group_by(market_id) |>
    summarise(market_profit = sum(profit), .groups = "drop") |>
    left_join(
      res |> distinct(market_id, firm_label, display_name) |>
        pivot_wider(names_from = firm_label, values_from = display_name),
      by = "market_id") |>
    arrange(desc(market_profit))

  list(individuals = as.data.frame(indiv), markets = as.data.frame(markets),
       ne_profit_per_round = ne_pr, rounds = nr)
}
