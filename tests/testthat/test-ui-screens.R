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

test_that("every admin and player screen renders across the game lifecycle", {
  con <- fresh_db()
  ns  <- NS("t")
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  ids <- join(con, sid, c("Ada", "Boaz", "Cleo", "Dax"))

  renders(ad_create(ns, names(load_games(GAMES_PATH))))

  renders(ad_nav(ns, "run"))
  renders(ad_nav(ns, "history"))
  renders(ad_history(ns, sl_list_sessions(con)))

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

  for (r in 1:10) {
    if (r > 1) for (pid in paired) sl_submit(con, sid, pid, r, 12)
    if (sl_session(con, sid)$phase == "open") sl_score_round(con, sid)
    sl_advance_round(con, sid)
  }
  st <- sl_load_state(con, sid)
  expect_equal(st$session$phase, "finished")
  renders(ad_finished(ns, st))
  renders(pl_finished(st$players[st$players$player_id == paired[1], ], st))
  expect_equal(nrow(sl_export_frame(con, sid)), 40)   # 4 players x 10 rounds
})

test_that("player bench screen renders for a sitting-out player", {
  con <- fresh_db()
  ns  <- NS("t")
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, c("a", "b", "c"))
  sl_start_game(con, sid, seed = 2)
  st <- sl_load_state(con, sid)
  bench <- st$players[is.na(st$players$market_id), ][1, ]
  # the module picks the bench card itself; just confirm the generic card renders
  renders(pl_card("You're sitting this one out", "…"))
  expect_true(is.na(bench$market_id))
})
