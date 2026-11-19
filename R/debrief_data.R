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

  predictions <- db_query(con,
    "SELECT pr.player_id, p.display_name, pr.qid, pr.answer
     FROM predictions pr JOIN players p ON p.player_id = pr.player_id
     WHERE pr.session_id = ?", list(session_id))

  list(session = s, game = game,
       model = game$demand$model, params = game$demand$params,
       benchmarks = game$benchmarks,
       players = players, results = results, submissions = submissions,
       predictions = predictions)
}

# Same shape, from an unzipped export bundle (for offline use in debrief.qmd).
dbf_load_bundle <- function(dir, session_id = NULL) {
  rd <- function(f) if (file.exists(file.path(dir, f)))
    utils::read.csv(file.path(dir, f), stringsAsFactors = FALSE) else NULL
  bm  <- rd("benchmarks.csv")
  if (is.null(session_id)) session_id <- bm$session_id[1]
  bm  <- bm[bm$session_id == session_id, ][1, ]
  res <- rd("results_long.csv"); res <- res[res$session_id == session_id, ]
  sub <- rd("submissions.csv");  sub <- sub[sub$session_id == session_id, ]
  prd <- rd("predictions.csv")
  prd <- if (is.null(prd)) data.frame() else prd[prd$session_id == session_id, ]
  params <- list(V = bm$V, t = bm$t, L = bm$L, M = bm$M, mc = bm$mc,
                 choke = bm$choke, coverage_decay = isTRUE(as.logical(bm$coverage_decay)))
  params <- params[!vapply(params, function(x) length(x) == 0 || is.na(x), logical(1))]
  list(
    session = list(session_id = session_id, n_rounds = bm$rounds, phase = bm$phase),
    game = list(label = bm$game_label, demand = list(model = bm$model, params = params)),
    model = bm$model, params = params,
    benchmarks = list(ne = bm$ne, collusive = bm$collusive, monopoly = bm$monopoly),
    players = unique(res[c("market_id", "firm_label", "display_name")]),
    results = res, submissions = sub, predictions = prd)
}

# The same data with every student's name replaced by their market and firm
# ("Market 2, Firm A"), for the Debrief tab's tables when names are hidden.
dbf_anonymize <- function(d) {
  lab <- function(m, f) ifelse(is.na(m), "(no market)", sprintf("Market %s, %s", m, f))
  if (!is.null(d$results) && nrow(d$results))
    d$results$display_name <- lab(d$results$market_id, d$results$firm_label)
  if (!is.null(d$submissions) && nrow(d$submissions))
    d$submissions$display_name <- lab(d$submissions$market_id, d$submissions$firm_label)
  if (!is.null(d$players) && nrow(d$players)) {
    old <- d$players$display_name
    d$players$display_name <- lab(d$players$market_id, d$players$firm_label)
    if (!is.null(d$predictions) && nrow(d$predictions)) {
      i <- if (all(c("player_id") %in% names(d$predictions)) && "player_id" %in% names(d$players))
             match(d$predictions$player_id, d$players$player_id)
           else match(d$predictions$display_name, old)
      d$predictions$display_name <- d$players$display_name[i]
    }
  }
  d
}

# --- teachable moments ---------------------------------------------------------

.flags_cols <- c("kind", "market_id", "round", "firm_label", "who", "detail",
                 "rationale", "score")
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
  m2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")   # to the cent
  out <- list()

  swing <- res |>
    arrange(player_id, round) |>
    group_by(player_id) |>
    mutate(dp = profit - lag(profit)) |>
    ungroup() |>
    filter(!is.na(dp), abs(dp) >= 0.005) |>            # an unchanged profit is no swing
    mutate(score = abs(dp))
  if (nrow(swing)) {
    swing <- slice_max(swing, score, n = top_n, with_ties = FALSE)
    out$swing <- data.frame(kind = "Profit swing", market_id = swing$market_id,
      round = swing$round, firm_label = swing$firm_label, who = swing$display_name,
      detail = sprintf("%s's round profit %s $%s vs the round before",
                       swing$display_name,
                       ifelse(swing$dp >= 0, "jumped", "fell"), m2(abs(swing$dp))),
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
                       brs$display_name, m2(brs$price), m2(brs$rival_price), m2(brs$br)),
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
                         m2(ne), w$round[col$start], w$round[col$end]),
        score = col$len)
    cas <- .longest_true_run(diff(a) < 0 & diff(b) < 0)
    if (cas$len >= 3)
      out[[paste0("cas", mk)]] <- data.frame(kind = "Undercut cascade", market_id = mk,
        round = w$round[cas$end + 1], firm_label = "both", who = "both firms",
        detail = sprintf("both firms cut price %d rounds running (through round %d)",
                         cas$len, w$round[cas$end + 1]),
        score = cas$len)
  }

  # a firm whose rival priced above the monopoly price, or below the Nash price:
  # the two cases where the best response says something students can check
  ab   <- .ab_rows(d)
  mono <- d$benchmarks$monopoly %||% NA_real_
  rival_flag <- function(rows, gap, kind, where) {
    if (!nrow(rows)) return(NULL)
    rows$score <- gap
    rows <- slice_max(rows, score, n = top_n, with_ties = FALSE)
    br <- vapply(rows$rival_price,
                 function(pr) best_response(d$model, d$params, pr)$price, numeric(1))
    data.frame(kind = kind, market_id = rows$market_id, round = rows$round,
      firm_label = rows$firm_label, who = rows$display_name,
      detail = sprintf("%s's rival priced $%s, %s. %s priced $%s against a best response of $%s",
                       rows$display_name, m2(rows$rival_price), where,
                       rows$display_name, m2(rows$price), m2(br)),
      score = rows$score)
  }
  if (nrow(ab) && is.finite(mono)) {
    hi <- ab[ab$rival_price > mono + 1e-6, , drop = FALSE]
    out$above <- rival_flag(hi, hi$rival_price - mono, "Rival above monopoly",
                            sprintf("above the monopoly price ($%s)", m2(mono)))
  }
  if (nrow(ab) && is.finite(ne)) {
    lo <- ab[ab$rival_price < ne - 1e-6, , drop = FALSE]
    out$below <- rival_flag(lo, ne - lo$rival_price, "Rival below NE",
                            sprintf("below the Nash price ($%s)", m2(ne)))
  }

  res_df <- if (length(out)) do.call(rbind, out) else .flags_empty()
  res_df$rationale <- .flag_rationale(res_df, d$submissions)
  res_df[order(-res_df$score), , drop = FALSE]
}

# Firm A / Firm B rows that priced against a real rival: shadows (scored against
# a paired student's prices) and no-shows are left out.
.ab_rows <- function(d) {
  res <- d$results
  nos <- res$no_submission %||% rep(0, nrow(res))
  keep <- res$firm_label %in% c("Firm A", "Firm B") & is.finite(res$price) &
          is.finite(res$rival_price) & (is.na(nos) | nos == 0)
  res[keep, , drop = FALSE]
}

# The firm-round whose rival priced furthest above the monopoly price
# ("above_monopoly") or furthest below the Nash price ("below_ne"). When no
# rival did, the nearest case (the highest or lowest rival price), with
# exact = FALSE so a caption can say so. NULL when nothing was scored.
dbf_rival_case <- function(d, kind = c("above_monopoly", "below_ne")) {
  kind <- match.arg(kind)
  ab <- .ab_rows(d)
  b  <- d$benchmarks
  ref <- if (kind == "above_monopoly") b$monopoly %||% NA_real_ else b$ne %||% NA_real_
  if (!nrow(ab) || !is.finite(ref)) return(NULL)
  gap <- if (kind == "above_monopoly") ab$rival_price - ref else ref - ab$rival_price
  i <- order(-gap, ab$round, ab$market_id, ab$firm_label)[1]
  list(market_id = ab$market_id[i], round = ab$round[i], firm_label = ab$firm_label[i],
       price = ab$price[i], rival_price = ab$rival_price[i], exact = gap[i] > 1e-6)
}

# Markets where a firm cut its price in the last round played, biggest cut
# first: exact = TRUE where both firms priced above the Nash price the round
# before and the cut is positive (the end-of-game undercut), then the other
# markets by the size of their cut. At most `n` rows; none for a one-round game.
dbf_last_round_cuts <- function(d, n = 2L) {
  empty <- data.frame(market_id = integer(0), cut = numeric(0), exact = logical(0))
  ab <- d$results[d$results$firm_label %in% c("Firm A", "Firm B"), , drop = FALSE]
  if (!nrow(ab)) return(empty)
  last <- max(ab$round)
  if (last < 2) return(empty)
  ne <- d$benchmarks$ne %||% 0
  rows <- lapply(split(ab, ab$market_id), function(m) {
    prev <- m$price[m$round == last - 1]; now <- m$price[m$round == last]
    firm_prev <- m$firm_label[m$round == last - 1]; firm_now <- m$firm_label[m$round == last]
    if (length(prev) != 2 || length(now) != 2) return(NULL)
    drop <- prev[order(firm_prev)] - now[order(firm_now)]
    if (!any(is.finite(drop))) return(NULL)
    data.frame(market_id = m$market_id[1], cut = max(drop, na.rm = TRUE),
               held = all(is.finite(prev)) && all(prev > ne))
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || !nrow(df)) return(empty)
  df$exact <- df$held & df$cut > 0
  df <- df[order(!df$exact, -df$cut, df$market_id), c("market_id", "cut", "exact")]
  rownames(df) <- NULL
  utils::head(df, n)
}

# Markets ranked by the two firms' total profit over the game, highest first
# (a shadow's profit, scored against its host's prices, is left out).
dbf_market_ranks <- function(d) {
  res <- d$results[d$results$firm_label %in% c("Firm A", "Firm B"), , drop = FALSE]
  if (!nrow(res))
    return(data.frame(market_id = integer(0), joint_profit = numeric(0), rank = integer(0)))
  tot <- tapply(res$profit, res$market_id, sum, na.rm = TRUE)
  df  <- data.frame(market_id = as.integer(names(tot)), joint_profit = as.numeric(tot))
  df  <- df[order(-df$joint_profit, df$market_id), , drop = FALSE]
  df$rank <- seq_len(nrow(df))
  rownames(df) <- NULL
  df
}

# The student's own "why this price?" note for a flagged (player, round), where
# the flag names one firm; blank for the market-level flags ("both firms").
.flag_rationale <- function(fr, subs) {
  if (!nrow(fr)) return(character(0))
  vapply(seq_len(nrow(fr)), function(i) {
    if (!isTRUE(fr$firm_label[i] %in% c("Firm A", "Firm B")) ||
        is.null(subs) || !nrow(subs)) return("")
    hit <- subs[subs$display_name == fr$who[i] & subs$round == fr$round[i], ]
    if (nrow(hit) && !is.na(hit$rationale[1])) trimws(hit$rationale[1]) else ""
  }, character(1))
}

# --- leaderboards -----------------------------------------------------------

# --- predictions -----------------------------------------------------------

# One entry per prediction question: the question metadata, the class's answers,
# a computed class-level "actual" where we have one, and a per-student
# predicted-vs-actual frame for the "your round-1 price" question.
dbf_predictions <- function(d) {
  pr <- d$predictions
  if (is.null(pr) || !nrow(pr)) return(list())
  qs <- game_predictions(d$game)

  qmeta <- function(qid) {
    hit <- Filter(function(q) identical(q$id, qid), qs)
    if (length(hit)) return(hit[[1]])
    row <- pr[pr$qid == qid, ][1, ]
    list(id = qid, type = row$type %||% "text", prompt = row$prompt %||% qid,
         reference = suppressWarnings(as.numeric(row$reference %||% NA)), choices = NULL)
  }

  res <- d$results
  final_round <- suppressWarnings(max(res$round, na.rm = TRUE))
  class_final <- mean(res$price[res$round == final_round & !is.na(res$price)])
  r1_price <- function(nm) { v <- res$price[res$display_name == nm & res$round == 1]
                             if (length(v)) v[1] else NA_real_ }

  lapply(unique(pr$qid), function(qid) {
    q  <- qmeta(qid)
    aa <- pr[pr$qid == qid, c("display_name", "answer")]
    aa$answer_num <- suppressWarnings(as.numeric(aa$answer))
    ref <- q$reference %||% NA_real_
    actual <- switch(qid,
      class_final = class_final,
      eq_guess    = if (is.finite(ref)) ref else d$benchmarks$ne,
      ne_guess    = if (is.finite(ref)) ref else d$benchmarks$ne,
      if (is.finite(ref)) ref else NA_real_)
    list(
      question = q,
      answers  = aa,
      actual   = actual,
      per_student = if (identical(qid, "own_r1"))
        data.frame(display_name = aa$display_name, predicted = aa$answer_num,
                   actual = vapply(aa$display_name, r1_price, numeric(1)),
                   stringsAsFactors = FALSE)
        else NULL)
  })
}

# Wide per-student table: every prediction + their actual round-1 price + profit.
dbf_prediction_table <- function(d) {
  pr <- d$predictions
  if (is.null(pr) || !nrow(pr)) return(data.frame())
  w <- stats::reshape(pr[c("display_name", "qid", "answer")],
                      idvar = "display_name", timevar = "qid", direction = "wide")
  names(w) <- sub("^answer\\.", "", names(w))
  res <- d$results
  w$actual_r1 <- vapply(w$display_name, function(nm) {
    v <- res$price[res$display_name == nm & res$round == 1]
    if (length(v)) v[1] else NA_real_
  }, numeric(1))
  w$total_profit <- vapply(w$display_name, function(nm)
    sum(res$profit[res$display_name == nm], na.rm = TRUE), numeric(1))
  w[order(-w$total_profit), ]
}

# The projector's "Price history" view: one row per player and one column per
# scored round, the price each player was scored at (as the 2025 Google Sheet's
# Pricing tab showed every student's prices). Rows run by market, Firm A then
# Firm B then any shadow, so rivals sit on adjacent rows. A no-show's round is
# NA. A shadow's name carries "(shadow)", since its market then has three rows.
# `anon` puts the firm label where the name would be. The projector formats the
# prices (money2).
dbf_price_history <- function(d, anon = FALSE) {
  res <- d$results
  if (is.null(res) || !nrow(res)) return(NULL)
  rounds <- sort(unique(res$round))
  keys <- res[!duplicated(res$player_id),
              c("player_id", "market_id", "firm_label", "display_name"), drop = FALSE]
  keys <- keys[order(keys$market_id,
                     match(keys$firm_label, c("Firm A", "Firm B", "Shadow")),
                     keys$player_id), , drop = FALSE]
  out <- data.frame(Market = as.character(keys$market_id),
                    Player = if (anon) keys$firm_label
                             else ifelse(keys$firm_label == "Shadow",
                                         paste(keys$display_name, "(shadow)"),
                                         keys$display_name),
                    stringsAsFactors = FALSE)
  for (r in rounds) {
    rr <- res[res$round == r, , drop = FALSE]
    out[[sprintf("Round %d", r)]] <- rr$price[match(keys$player_id, rr$player_id)]
  }
  rownames(out) <- NULL
  out
}

dbf_leaderboards <- function(d) {
  res <- d$results
  # the rounds scored so far: a game still running (or ended early) is compared
  # with the benchmark over the rounds actually played, not the full horizon
  nr  <- length(unique(res$round))
  ne  <- d$benchmarks$ne %||% 0
  coll <- d$benchmarks$collusive %||% d$benchmarks$monopoly %||% ne
  ne_pr   <- own_profit(d$model, d$params, ne, ne)        # symmetric per-round NE profit
  coll_pr <- own_profit(d$model, d$params, coll, coll)    # ...and if both had colluded

  indiv <- res |>
    group_by(market_id, firm_label, display_name) |>
    summarise(total_profit = sum(profit), .groups = "drop") |>
    mutate(vs_ne = total_profit - nr * ne_pr,
           vs_collusive = total_profit - nr * coll_pr) |>
    arrange(desc(total_profit))

  # a market's profit is its two firms'; a shadow is scored in a market of its
  # own against the host's prices, so its profit is not this market's
  markets <- res |>
    filter(firm_label %in% c("Firm A", "Firm B")) |>
    group_by(market_id) |>
    summarise(market_profit = sum(profit), .groups = "drop") |>
    left_join(
      res |> distinct(market_id, firm_label, display_name) |>
        pivot_wider(names_from = firm_label, values_from = display_name),
      by = "market_id") |>
    arrange(desc(market_profit))

  list(individuals = as.data.frame(indiv), markets = as.data.frame(markets),
       ne_profit_per_round = ne_pr, collusive_profit_per_round = coll_pr,
       rounds = nr)
}
