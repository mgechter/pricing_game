# Debrief data + plots (shared by the Shiny tab and debrief.qmd).

suppressMessages(library(ggplot2))

test_that(".longest_true_run finds the longest consecutive TRUE block", {
  r <- .longest_true_run(c(F, T, T, F, T, T, T, T, F))
  expect_equal(r$len, 4); expect_equal(r$start, 5); expect_equal(r$end, 8)
  expect_equal(.longest_true_run(c(F, F))$len, 0)
})

test_that("dbf_load returns the expected shape", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, 120)

  d <- dbf_load(con, sid)
  expect_equal(d$model, "hotelling_linear")
  expect_equal(d$benchmarks$ne, 100)
  expect_true(nrow(d$results) > 0)
  expect_true(all(c("round", "market_id", "firm_label", "price", "profit") %in% names(d$results)))
})

test_that("dbf_flags catches tacit collusion and an off-best-response move", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)   # NE = 0
  join(con, sid, c("A", "B")); sl_start_game(con, sid, seed = 1)
  # both firms hold well above NE every round -> a collusion run;
  # in round 2, Firm A jumps to a wild price (far from its best response of ~just under 25)
  play_out(con, sid, function(round, firm, pid) {
    if (round == 2 && firm == "Firm A") 95 else 25
  })
  fr <- dbf_flags(dbf_load(con, sid))
  expect_true("Tacit collusion" %in% fr$kind)
  expect_true("Off best response" %in% fr$kind)
  expect_true("rationale" %in% names(fr))
})

test_that("a rival above monopoly or below NE is flagged and found", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)               # NE 0, monopoly 50
  join(con, sid, c("A", "B")); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) if (f == "Firm A") 46.4 else 52.1 + r)
  d  <- dbf_load(con, sid)
  fr <- dbf_flags(d)
  hi <- fr[fr$kind == "Rival above monopoly", ]
  expect_equal(nrow(hi), 2)                               # top_n of four rounds
  expect_true(all(hi$firm_label == "Firm A"))
  expect_equal(sort(hi$round), c(3, 4))                   # Firm B's two highest prices
  expect_match(hi$detail[1], "best response of $50.00", fixed = TRUE)
  expect_false("Rival below NE" %in% fr$kind)

  above <- dbf_rival_case(d, "above_monopoly")
  expect_true(above$exact)
  expect_equal(above$firm_label, "Firm A"); expect_equal(above$round, 4)
  expect_equal(above$rival_price, 56.1)
  below <- dbf_rival_case(d, "below_ne")                  # no price below 0: nearest case
  expect_false(below$exact)
  expect_equal(below$rival_price, 46.4); expect_equal(below$firm_label, "Firm B")

  con2 <- fresh_db()
  sid2 <- sl_create_session(con2, "hotelling_high", GAMES_PATH)       # NE 100
  join(con2, sid2, c("A", "B")); sl_start_game(con2, sid2, seed = 1)
  play_out(con2, sid2, function(r, f, p) if (f == "Firm A") 90 else 120)
  d2 <- dbf_load(con2, sid2)
  lo <- dbf_flags(d2); lo <- lo[lo$kind == "Rival below NE", ]
  expect_true(nrow(lo) >= 1); expect_true(all(lo$firm_label == "Firm B"))
  expect_true(dbf_rival_case(d2, "below_ne")$exact)
  # the Figure pack draws the new flags' firm-rounds too
  files <- debrief_write_figs(d2, tempfile("figs_"))
  expect_true(any(grepl(sprintf("_r%d_B\\.png$", lo$round[1]), files)))
})

test_that("dbf_last_round_cuts finds the end-of-game undercuts, biggest first", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)          # NE 50
  ids <- join(con, sid, sprintf("P%d", 1:8)); sl_start_game(con, sid, seed = 4)
  nr  <- sl_session(con, sid)$n_rounds
  # everyone holds $200; in the last round each player cuts by 10 x their index
  play_out(con, sid, function(r, f, p) if (r < nr) 200 else 200 - 10 * match(p, ids))
  d <- dbf_load(con, sid)
  res <- d$results[d$results$firm_label %in% c("Firm A", "Firm B"), ]
  want <- vapply(split(res, res$market_id), function(m)
    max(m$price[m$round == nr - 1]) - min(m$price[m$round == nr]), numeric(1))
  got <- dbf_last_round_cuts(d, 2)
  expect_equal(nrow(got), 2)
  expect_true(all(got$exact))
  expect_equal(got$market_id, as.integer(names(sort(want, decreasing = TRUE))[1:2]))
  expect_equal(got$cut, unname(sort(want, decreasing = TRUE)[1:2]))

  # a game ended after round 1 has no last-round cut to show
  con1 <- fresh_db()
  s1 <- sl_create_session(con1, "hotelling_low", GAMES_PATH)
  join(con1, s1, sprintf("P%d", 1:4)); sl_start_game(con1, s1, seed = 1)
  pl <- sl_players(con1, s1); pl <- pl[!is.na(pl$market_id), ]
  for (pid in pl$player_id) sl_submit(con1, s1, pid, 1, 120)
  sl_score_round(con1, s1); sl_end_early(con1, s1)
  expect_equal(nrow(dbf_last_round_cuts(dbf_load(con1, s1))), 0)
})

test_that("dbf_flags carries each flagged firm's own rationale", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("A", "B")); sl_start_game(con, sid, seed = 1)
  repeat {
    s <- sl_session(con, sid); if (s$phase == "finished") break
    pl <- sl_players(con, sid); pl <- pl[!is.na(pl$market_id), ]
    for (i in seq_len(nrow(pl))) {
      why <- if (s$current_round == 2 && pl$firm_label[i] == "Firm A")
        "panic move, wanted to grab the whole market" else "hold the line"
      p <- if (s$current_round == 2 && pl$firm_label[i] == "Firm A") 95 else 25
      sl_submit(con, sid, pl$player_id[i], s$current_round, p, rationale = why)
    }
    sl_score_round(con, sid); sl_advance_round(con, sid)
  }
  fr <- dbf_flags(dbf_load(con, sid))
  br <- fr[fr$kind == "Off best response", ]
  expect_true(nrow(br) >= 1)
  expect_match(br$rationale[1], "panic move")
  expect_equal(fr$rationale[fr$firm_label == "both"], rep("", sum(fr$firm_label == "both")))
})

test_that("dbf_leaderboards totals and vs-NE line up", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 3)
  play_out(con, sid, 150)

  d  <- dbf_load(con, sid)
  lb <- dbf_leaderboards(d)
  res <- d$results
  for (i in seq_len(nrow(lb$individuals))) {
    nm <- lb$individuals$display_name[i]
    expect_equal(lb$individuals$total_profit[i],
                 sum(res$profit[res$display_name == nm]))
  }
  nr <- d$session$n_rounds
  expect_equal(lb$individuals$vs_ne,
               lb$individuals$total_profit - nr * lb$ne_profit_per_round)
  expect_equal(lb$individuals$vs_collusive,
               lb$individuals$total_profit - nr * lb$collusive_profit_per_round)
  # colluding every round pays more than NE every round -> vs_collusive is worse
  expect_true(all(lb$individuals$vs_collusive <= lb$individuals$vs_ne + 1e-6))
})

test_that("plot builders return ggplot objects; best-response guards bad input", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) 90 + 10 * r)
  d <- dbf_load(con, sid)

  expect_s3_class(plot_price_paths(d), "ggplot")
  expect_s3_class(plot_price_paths(d, anon = TRUE), "ggplot")
  expect_s3_class(plot_avg_price(d), "ggplot")
  expect_s3_class(plot_profit_paths(d), "ggplot")
  expect_s3_class(plot_best_response(d, min(d$results$market_id), 2, "Firm A"), "ggplot")
  expect_error(plot_best_response(d, 999, 2, "Firm A"), "isn't available")
})

test_that("price paths name each pair, label the benchmarks, and draw chosen markets", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)        # NE 50, monopoly 225
  join(con, sid, c("Ada", "Boaz", "Cleo", "Dmitri", "Esme"))         # odd -> a shadow
  sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) 60 + r)
  d   <- dbf_load(con, sid)
  mks <- sort(unique(d$results$market_id))

  p <- plot_price_paths(d)
  strips <- levels(p$data$market)
  expect_length(strips, length(mks))
  expect_true(all(grepl("^Market \\d+: .+ \\(A\\) vs .+ \\(B\\)", strips)))
  expect_equal(sum(grepl(", shadow ", strips)), 1)
  expect_setequal(as.character(unique(p$data$firm)), c("Firm A", "Firm B", "Shadow"))
  bench <- p$layers[[2]]$data$text
  expect_true(all(c("NE $50.00", "Monopoly $225.00") %in% bench))

  one <- plot_price_paths(d, markets = mks[2])
  expect_equal(unique(one$data$market_id), mks[2])
  expect_length(levels(one$data$market), 1)

  anon <- plot_price_paths(d, anon = TRUE)
  expect_true(all(grepl("^Market \\d+$", levels(anon$data$market))))
  expect_false(any(grepl("Ada|Boaz|Cleo|Dmitri|Esme", levels(anon$data$market))))

  expect_error(plot_price_paths(d, markets = 999), "none of those markets")
})

# a plot's title / subtitle, under ggplot2 3.5 and 4.0 alike
plot_lab <- function(p, k)
  if (exists("get_labs", asNamespace("ggplot2"))) ggplot2::get_labs(p)[[k]] else p$labels[[k]]
layer_text <- function(p)
  Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]$data$text

test_that("best-response figure labels each line with its value and says where the rival priced", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)               # NE 0, monopoly 50
  join(con, sid, c("Ada", "Boaz")); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) if (f == "Firm A") 46.4 else 52.1)
  d  <- dbf_load(con, sid)
  mk <- unique(d$results$market_id)
  who <- function(f) d$results$display_name[d$results$firm_label == f][1]

  p <- plot_best_response(d, mk, 1, "Firm A")
  expect_setequal(layer_text(p),
                  c("Actual $46.40", "Best response $50.00", "NE $0.00", "Monopoly $50.00"))
  expect_match(plot_lab(p, "title"), paste0("^", who("Firm A"), "'s round 1 profit"))
  expect_match(plot_lab(p, "subtitle"), "above the monopoly price ($50.00)", fixed = TRUE)
  expect_match(plot_lab(p, "subtitle"), who("Firm B"), fixed = TRUE)

  q <- plot_best_response(d, mk, 1, "Firm B")
  expect_match(plot_lab(q, "subtitle"), "between the Nash ($0.00) and monopoly ($50.00) prices",
               fixed = TRUE)
  expect_match(plot_lab(plot_best_response(d, mk, 1, "Firm A", anon = TRUE), "title"), "^Firm A's")
})

test_that("both firms of a market stack in one figure, chosen firm on top", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)          # monopoly 225
  join(con, sid, c("Ada", "Boaz", "Cleo")); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) if (f == "Firm A") 230 else 110)
  d  <- dbf_load(con, sid)
  mk <- unique(d$results$market_id[d$results$firm_label == "Shadow"])
  b_name <- d$results$display_name[d$results$market_id == mk & d$results$firm_label == "Firm B"][1]

  q <- plot_best_response_pair(d, mk, 2, first = "Firm B")
  strips <- levels(q$data$panel)
  expect_length(strips, 2)
  expect_match(strips[1], paste0("^", b_name, ", given "))
  expect_match(strips[1], "above the monopoly price ($225.00)", fixed = TRUE)
  expect_length(layer_text(q), 8)                       # four lines in each panel
  expect_error(plot_best_response(d, mk, 2, "Shadow"), "isn't available")
  expect_error(plot_best_response_pair(d, 999, 2), "isn't available")
})

test_that("price quartiles by round, and one market's prices and profits ranked by joint profit", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)         # NE 100, monopoly 200
  ids <- join(con, sid, c("Ada", "Boaz", "Cleo", "Dmitri", "Esme"))  # odd -> a shadow
  sl_start_game(con, sid, seed = 3)
  play_out(con, sid, function(r, f, p) 80 + 10 * match(p, ids) + r)
  d <- dbf_load(con, sid)

  q <- plot_price_quartiles(d)
  r3 <- d$results$price[d$results$round == 3 & is.finite(d$results$price)]
  got <- q$data[q$data$round == 3, ]
  expect_equal(got$price[got$stat == "Median"], stats::median(r3))
  expect_equal(got$price[got$stat == "25th percentile"], unname(stats::quantile(r3, 0.25)))
  expect_equal(nrow(q$data), 3 * length(unique(d$results$round)))
  expect_true(all(c("NE $100.00", "Monopoly $200.00") %in% layer_text(q)))

  rk  <- dbf_market_ranks(d)
  ab  <- d$results[d$results$firm_label %in% c("Firm A", "Firm B"), ]
  want <- sort(tapply(ab$profit, ab$market_id, sum), decreasing = TRUE)
  expect_equal(rk$market_id, as.integer(names(want)))
  expect_equal(rk$joint_profit, unname(as.numeric(want)))
  expect_equal(rk$rank, seq_len(nrow(rk)))

  top <- rk$market_id[1]
  m <- plot_market_detail(d, top)
  expect_setequal(levels(m$data$measure), c("Price ($)", "Round profit ($)"))
  expect_match(plot_lab(m, "title"), sprintf("^Market %d: .+ \\(A\\) vs .+ \\(B\\)", top))
  expect_match(plot_lab(m, "subtitle"), sprintf("^Rank 1 of %d markets", nrow(rk)))
  expect_match(plot_lab(plot_market_detail(d, top, anon = TRUE), "title"), sprintf("^Market %d$", top))
  expect_error(plot_market_detail(d, 999), "isn't available")

  files <- debrief_write_figs(d, tempfile("figs_"))
  expect_true(any(grepl("price_quartiles\\.png$", files)))
  expect_equal(sum(grepl("^market_rank\\d+_m\\d+\\.png$", basename(files))), nrow(rk))
  expect_true(all(file.exists(files)))
})

test_that("best-response explorer works in a market with a shadow player", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:5)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, function(r, f, p) 40 - 5 * r)
  d  <- dbf_load(con, sid)
  mk <- unique(d$results$market_id[d$results$firm_label == "Shadow"])
  expect_length(mk, 1)
  for (f in c("Firm A", "Firm B"))
    expect_s3_class(plot_best_response(d, mk, 2, f), "ggplot")
})

test_that("dbf_predictions summarises answers and computes actuals", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("A", "B", "C", "D"))
  sl_start_game(con, sid, seed = 1)
  g <- sl_game(sl_session(con, sid))
  for (pid in ids) for (q in game_predictions(g))
    sl_set_prediction(con, sid, pid, q$id,
                      if (q$type == "numeric") as.character(30 + pid)
                      else if (q$type == "choice") q$choices[1] else "because")
  play_out(con, sid, 10)

  d  <- dbf_load(con, sid)
  it <- dbf_predictions(d)
  ids_seen <- vapply(it, function(x) x$question$id, "")
  expect_true(all(c("own_r1", "eq_guess", "br_50", "can_hold") %in% ids_seen))

  eqg <- it[[which(ids_seen == "eq_guess")]]
  expect_equal(eqg$actual, 0)                      # bertrand NE / reference
  br  <- it[[which(ids_seen == "br_50")]]
  expect_equal(br$actual, 49)
  own <- it[[which(ids_seen == "own_r1")]]
  expect_true(!is.null(own$per_student))
  expect_true(all(own$per_student$actual == 10))   # everyone priced 10 in round 1

  expect_s3_class(plot_predictions_grid(it), "ggplot")

  tbl <- dbf_prediction_table(d)
  expect_true(all(c("own_r1", "can_hold", "actual_r1", "total_profit") %in% names(tbl)))
  expect_equal(nrow(tbl), 4)
})

test_that("debrief_write_figs writes the standard PNG set", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_high", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:6)); sl_start_game(con, sid, seed = 2)
  play_out(con, sid, function(r, f, p) round(runif(1, 60, 220)))
  d   <- dbf_load(con, sid)
  dir <- tempfile("figs_")
  files <- debrief_write_figs(d, dir)
  expect_true(length(files) >= 4)
  expect_true(all(file.exists(files)))
  expect_true(any(grepl("price_paths\\.png$", files)))
})

test_that("debrief_write_figs writes one best-response PNG per flagged firm-round", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  # both firms of every market swing hard in round 2, so several flags share a
  # market and round (the case that used to overwrite one PNG with another)
  play_out(con, sid, function(r, f, p) if (r == 2) (if (f == "Firm A") 5 else 45) else 30)
  d  <- dbf_load(con, sid)
  fr <- dbf_flags(d)
  fr <- fr[fr$firm_label %in% c("Firm A", "Firm B"), ]
  want <- nrow(unique(fr[c("market_id", "round", "firm_label")]))
  files <- debrief_write_figs(d, tempfile("figs_"))
  br <- files[grepl("^best_response_", basename(files))]
  expect_true(want >= 2)
  expect_length(br, want)
  expect_false(anyDuplicated(basename(files)) > 0)
  expect_true(all(file.exists(br)))
  expect_true(all(grepl("_[AB]\\.png$", br)))
})

test_that("dbf_load_bundle round-trips through the export files", {
  con <- fresh_db()
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  play_out(con, sid, 130)

  dir <- tempfile("bundle_"); dir.create(dir)
  b <- sl_export_bundle(con, sid)
  for (nm in names(b)) utils::write.csv(b[[nm]], file.path(dir, paste0(nm, ".csv")),
                                        row.names = FALSE, na = "")
  db <- dbf_load_bundle(dir)
  live <- dbf_load(con, sid)
  expect_equal(db$benchmarks$ne, live$benchmarks$ne)
  expect_equal(nrow(db$results), nrow(live$results))
  expect_s3_class(plot_price_paths(db), "ggplot")
})
