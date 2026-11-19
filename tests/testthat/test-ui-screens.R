# The Shiny layer is thin glue over session_logic; there is no headless browser
# here, so these just confirm every screen builder renders against real game
# state without error (catches column-name / template drift).

testthat::skip_if_not_installed("shiny")
suppressMessages(library(shiny))
source(file.path(normalizePath(file.path(getwd(), "..", "..")), "R", "ui_helpers.R"))
source(file.path(normalizePath(file.path(getwd(), "..", "..")), "R", "mod_player.R"))
source(file.path(normalizePath(file.path(getwd(), "..", "..")), "R", "mod_admin.R"))

renders <- function(tag) {
  h <- as.character(tag)
  expect_type(h, "character")
  expect_gt(nchar(h), 20)
}

test_that("money shows to the cent", {
  expect_equal(money2(c(19.5, 40, 1234.567, 1641.6, NA)),
               c("$19.50", "$40.00", "$1,234.57", "$1,641.60", "—"))
})

test_that("every admin and player screen renders across the game lifecycle", {
  con <- fresh_db()
  ns  <- NS("t")
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("Ada", "Boaz", "Cleo", "Dax"))

  renders(ad_create(ns, ad_game_choices(GAMES_PATH)))

  renders(ad_nav(ns, "run"))
  renders(ad_nav(ns, "history"))
  renders(ad_nav(ns, "debrief"))
  renders(ad_history(ns, sl_list_sessions(con)))
  renders(ad_debrief(ns, NULL, sl_list_sessions(con)[0, ], NULL, NULL))

  st <- sl_load_state(con, sid)
  renders(ad_lobby(ns, st, "http://host/"))
  renders(pl_join(ns, st))
  renders(pl_lobby(st$players[1, ], st))

  sl_start_game(con, sid, seed = 1)
  st <- sl_load_state(con, sid)
  paired <- st$players$player_id[!is.na(st$players$market_id)]
  renders(ad_round(ns, st, "open"))
  renders(pl_round(ns, st$players[st$players$player_id == paired[1], ], st, NULL))

  for (pid in paired) sl_submit(con, sid, pid, 1, 10 + pid)
  st  <- sl_load_state(con, sid)
  sub <- st$submissions[st$submissions$player_id == paired[1], ]
  renders(ad_round(ns, st, "open"))
  renders(pl_round(ns, st$players[st$players$player_id == paired[1], ], st, sub))

  sl_score_round(con, sid)
  st <- sl_load_state(con, sid)
  r1 <- st$results[st$results$player_id == paired[1] & st$results$round == 1, ]
  renders(ad_round(ns, st, "scored"))
  renders(pl_result(st$players[1, ], st, r1))

  nr <- sl_session(con, sid)$n_rounds
  repeat {
    s <- sl_session(con, sid)
    if (s$phase == "finished") break
    if (s$phase == "open") {
      for (pid in paired) sl_submit(con, sid, pid, s$current_round, 12)
      sl_score_round(con, sid)
    }
    sl_advance_round(con, sid)
  }
  st <- sl_load_state(con, sid)
  expect_equal(st$session$phase, "finished")
  renders(ad_finished(ns, st))
  renders(pl_finished(st$players[st$players$player_id == paired[1], ], st))
  expect_equal(nrow(sl_export_frame(con, sid)), 4 * nr)   # 4 players x nr rounds

  # debrief screen against the finished session
  d  <- dbf_load(con, sid)
  ss <- sl_list_sessions(con); ss <- ss[ss$n_result_rows > 0, ]
  renders(ad_debrief(ns, d, ss, sid, dbf_flags(d)))
  # with markets picked on the Price paths card (999 is no market of this game)
  mk <- sort(unique(d$results$market_id))
  html <- as.character(ad_debrief(ns, d, ss, sid, dbf_flags(d), c(mk[1], 999)))
  expect_match(html, "Markets to show", fixed = TRUE)
})

test_that("the Game menu shows each game's label, with its id as the value", {
  ch <- ad_game_choices(GAMES_PATH)
  expect_equal(unname(ch), names(load_games(GAMES_PATH)))
  expect_equal(names(ch)[unname(ch) == "bertrand"], "Bertrand (homogeneous good)")
  html <- as.character(ad_create(NS("t"), ch))
  expect_match(html, '<option value="bertrand" selected>Bertrand (homogeneous good)</option>',
               fixed = TRUE)
})

test_that("odd headcount: the shadow player still gets a round view", {
  con <- fresh_db()
  ns  <- NS("t")
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, c("a", "b", "c"))
  sl_start_game(con, sid, seed = 2)
  st <- sl_load_state(con, sid)
  sh <- st$players[!is.na(st$players$shadow_of), ][1, ]
  expect_false(is.na(sh$market_id))                  # in a market, not benched
  renders(pl_round(ns, sh, st, NULL))
})

test_that("prediction screen renders and gates pl_screen", {
  con <- fresh_db()
  ns  <- NS("t")
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("a", "b"))
  sl_start_game(con, sid, seed = 1)
  st  <- sl_load_state(con, sid)
  qs  <- game_predictions(st$game)
  renders(pl_predict(ns, qs, sl_predictions(con, sid, ids[1])))

  pl    <- st$players[st$players$player_id == ids[1], ]
  gated <- pl_screen(ns, st, pl, NULL, NULL,
                     pred = list(questions = qs, answers = sl_predictions(con, sid, ids[1])))
  expect_true(grepl("your predictions", as.character(gated)))
})
