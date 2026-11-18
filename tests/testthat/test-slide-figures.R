# ECON 30303's "Slide figures" tab (class-only, R/slide_figures.R).

root <- normalizePath(file.path(getwd(), "..", ".."))
source(file.path(root, "R", "slide_figures.R"))
source(file.path(root, "R", "ui_helpers.R"))
source(file.path(root, "R", "mod_admin.R"))
suppressMessages(library(ggplot2))

ids <- vapply(SLIDE_FIGURES, `[[`, "", "id")

play_game <- function(con, game, names, price) {
  sid <- sl_create_session(con, game, GAMES_PATH)
  join(con, sid, names); sl_start_game(con, sid, seed = 1)
  nr <- sl_session(con, sid)$n_rounds
  play_out(con, sid, function(r, f, p) price(r, f, nr))
  sid
}

# Three games in which each slide's case happens: a Bertrand rival above $50,
# a t = 1 rival below $100, a t = 0.5 rival above $225 and last-round cuts.
three_games <- function(con) {
  list(
    old  = play_game(con, "bertrand", c("A", "B"), function(r, f, nr) 30),
    bert = play_game(con, "bertrand", sprintf("P%d", 1:5),
                     function(r, f, nr) if (f == "Firm A") 46.4 else 52.1),
    high = play_game(con, "hotelling_high", sprintf("P%d", 1:4),
                     function(r, f, nr) if (f == "Firm A") 90 else 130),
    low  = play_game(con, "hotelling_low", sprintf("P%d", 1:6), function(r, f, nr)
      if (r == 2 && f == "Firm B") 230 else if (r < nr) 200 else if (f == "Firm A") 120 else 190))
}

test_that("every slide figure is drawn from the latest session of its game", {
  con <- fresh_db()
  s <- three_games(con)
  figs <- slide_figures_build(con)
  expect_named(figs, ids)
  for (x in figs) expect_s3_class(x$plot, "ggplot")
  # sessions made without a label: "Session #2: 4 rounds scored.", never "NA"
  expect_match(figs$bertrand_price_paths$source, sprintf("^Session #%d: 4 rounds scored\\.$", s$bert))
  expect_false(any(grepl("NA", vapply(figs, function(x) x$source %||% "", ""))))
  sl_set_label(con, s$low, "Section 002")
  expect_match(slide_figures_build(con)$hotelling_low_price_paths$source,
               sprintf("^Session #%d, Section 002: ", s$low))
  expect_match(figs$bertrand_rival_above_monopoly$note,
               "the rival priced $52.10, above the monopoly price ($50.00)", fixed = TRUE)
  expect_match(figs$hotelling_high_rival_below_ne$note,
               "the rival priced $90.00, below the Nash price ($100.00)", fixed = TRUE)
  expect_match(figs$hotelling_low_rival_above_monopoly_both_firms$note,
               "round 2: the rival priced $230.00, above the monopoly price ($225.00)", fixed = TRUE)
  expect_match(figs$hotelling_low_last_round_cuts$note,
               paste("In round 10, the last round played, the biggest cuts after both firms",
                     "had priced above the Nash price the round before: $80.00 in market"),
               fixed = TRUE)

  files <- slide_figures_write(figs, tempfile("sf_"))
  expect_setequal(basename(files), c(paste0(ids, ".png"), "captions.txt"))
  expect_true(all(file.size(files) > 0))
  cap <- readLines(files[basename(files) == "captions.txt"])
  expect_false(any(grepl("P[1-6]\\b", cap)))                # no player names in the captions
})

test_that("the 2024 figures: quartiles per game, and every market's prices and profits", {
  con <- fresh_db()
  three_games(con)
  ds   <- slide_figures_data(con)
  figs <- slide_figures_from(ds)
  q <- figs[vapply(figs, function(x) identical(x$set, "2024"), logical(1))]
  expect_named(q, c("bertrand_price_quartiles", "hotelling_high_price_quartiles",
                    "hotelling_low_price_quartiles"))
  for (x in q) expect_s3_class(x$plot, "ggplot")

  ch <- slide_market_choices(ds)
  expect_named(ch, c("Bertrand", "Differentiated products, t = 1",
                     "Differentiated products, t = 0.5"))
  n_markets <- sum(vapply(ds, function(d) nrow(dbf_market_ranks(d)), integer(1)))
  expect_length(unlist(ch), n_markets)
  one <- slide_market_figure(ds, ch[[3]][[1]])
  expect_s3_class(one$plot, "ggplot")
  expect_match(one$id, "^hotelling_low_rank1_market\\d+$")
  expect_null(slide_market_figure(ds, "hotelling_low|999"))
  expect_null(slide_market_figure(ds, "nonsense"))

  dir <- tempfile("sf_")
  files <- slide_figures_write(figs, dir, ds = ds)
  rel <- substring(files, nchar(dir) + 2)
  mk  <- rel[startsWith(rel, "markets/")]
  expect_length(mk, n_markets)
  expect_true(all(grepl("^markets/(bertrand|hotelling_high|hotelling_low)_rank\\d+_market\\d+\\.png$", mk)))
  expect_true(all(file.size(files) > 0))
  expect_true(any(readLines(file.path(dir, "captions.txt")) == "markets/"))
})

test_that("a missing game, a nearest case and a one-round game each get a note", {
  con <- fresh_db()
  play_game(con, "bertrand", c("A", "B"), function(r, f, nr) if (f == "Firm A") 30 else 35)
  sid <- sl_create_session(con, "hotelling_low", GAMES_PATH)
  join(con, sid, sprintf("P%d", 1:4)); sl_start_game(con, sid, seed = 1)
  pl <- sl_players(con, sid); pl <- pl[!is.na(pl$market_id), ]
  for (pid in pl$player_id) sl_submit(con, sid, pid, 1, 120)
  sl_score_round(con, sid); sl_end_early(con, sid)

  figs <- slide_figures_build(con)
  above <- figs$bertrand_rival_above_monopoly
  expect_s3_class(above$plot, "ggplot")
  expect_match(above$note, "No rival priced above the monopoly price ($50.00) in this game, so this is the nearest case",
               fixed = TRUE)
  expect_null(figs$hotelling_high_price_paths$plot)
  expect_match(figs$hotelling_high_price_paths$note, "No scored session of the hotelling_high game yet")
  expect_null(figs$hotelling_low_last_round_cuts$plot)
  expect_match(figs$hotelling_low_last_round_cuts$note, "one round")
  expect_s3_class(figs$hotelling_low_price_paths$plot, "ggplot")

  files <- slide_figures_write(figs, tempfile("sf_"))
  expect_true("captions.txt" %in% basename(files))
  expect_false(any(grepl("hotelling_high", basename(files))))
})

test_that("the Slide figures tab renders, and the admin server draws it on opening", {
  con <- fresh_db()
  three_games(con)
  ds <- slide_figures_data(con)
  figs <- slide_figures_from(ds); attr(figs, "markets") <- slide_market_choices(ds)
  html <- as.character(ad_slide_figures(shiny::NS("t"), figs))
  expect_match(html, "Download all (ZIP)", fixed = TRUE)
  for (id in ids) expect_match(html, paste0(id, ".png"), fixed = TRUE)
  expect_match(html, "One market's prices and profits", fixed = TRUE)
  expect_match(html, "Like the 2024 debrief", fixed = TRUE)
  expect_match(as.character(ad_nav(shiny::NS("t"), "figures")), "Slide figures", fixed = TRUE)

  shiny::testServer(admin_server, args = list(con = con, games_path = GAMES_PATH), {
    expect_null(slide_figs())                               # not drawn on another tab
    session$setInputs(nav_figures = 1)
    expect_named(slide_figs(), ids)
    expect_match(output$screen$html, "hotelling_low_last_round_cuts.png", fixed = TRUE)
    expect_match(output$screen$html, "rank 1 of", fixed = TRUE)   # the market picker
  })
})

# --- defects from the adversarial review (Codex, 2026-11-18), failing at 47bd021 ---

.sf_start <- function(con, game, n = 2L) {
  id <- sl_create_session(con, game, GAMES_PATH, ask_predictions = FALSE)
  join(con, id, paste0("Student ", seq_len(n)))
  sl_start_game(con, id, seed = 1)
  id
}
.sf_score <- function(con, id, price) {
  pl <- sl_players(con, id); r <- sl_session(con, id)$current_round
  for (i in seq_len(nrow(pl))) {
    p <- if (is.function(price)) price(pl[i, ], r) else price
    if (!is.na(p)) sl_submit(con, id, pl$player_id[i], r, p)
  }
  sl_score_round(con, id)
}

test_that("C14: the slide caption's two biggest cuts must really be the two biggest cuts", {
  con <- fresh_db(); csid <- .sf_start(con, "hotelling_low", n = 6)
  # Markets 1 and 2 meet the collusion filter, but market 3 has the largest
  # actual cut (200 -> 0). Its rival's prior 40 is below NE.
  .sf_score(con, csid, function(p, r) {
    if (p$market_id < 3) 100 else if (p$firm_label == "Firm A") 200 else 40
  })
  sl_advance_round(con, csid)
  .sf_score(con, csid, function(p, r) {
    if (p$market_id == 1) 90 else if (p$market_id == 2) 95 else 0
  })
  sl_end_early(con, csid)
  figure <- slide_figures_build(con)$hotelling_low_last_round_cuts
  # The caption must not call the displayed markets the "biggest price cuts"
  # while leaving out market 3's $200.
  expect_true(grepl("$200.00", figure$note, fixed = TRUE) ||
                !grepl("biggest price cut", figure$note, fixed = TRUE))
  expect_match(figure$note, "Not shown: a bigger cut of $200.00 in market 3", fixed = TRUE)
  expect_match(figure$note, "after both firms had priced above the Nash price", fixed = TRUE)
})

test_that("C15: missing last-round prices must not make a two-round game caption say one round", {
  con <- fresh_db(); csid <- .sf_start(con, "hotelling_low")
  .sf_score(con, csid, 150); sl_advance_round(con, csid)
  sl_score_round(con, csid); sl_end_early(con, csid)
  figure <- slide_figures_build(con)$hotelling_low_last_round_cuts
  expect_false(grepl("the game has one round", figure$note, fixed = TRUE))
  expect_match(figure$note, "no market has prices in both of its last two rounds", fixed = TRUE)
})
