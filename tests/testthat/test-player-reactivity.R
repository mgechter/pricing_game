# The student's price entry must survive both their own auto-saves and another
# student submitting. pl_screen_key() must be stable across ALL submission
# activity and change only on phase / round / pairing / result transitions, and
# the screen must be gated through a reactiveVal (value-diffed), not a plain
# reactive() (which re-invalidates downstream even when unchanged).

suppressMessages(library(shiny))
src <- function(f) source(file.path(normalizePath(file.path(getwd(), "..", "..")), "R", f))
src("ui_helpers.R"); src("mod_player.R")

st_open <- function(subs = NULL, round = 1, results = data.frame()) list(
  session = list(phase = if (nrow(results)) "scored" else "open",
                 current_round = round, session_id = 1),
  submissions = subs %||% data.frame(player_id = integer(), price = numeric(),
                                     rationale = character(), submitted_at = numeric()),
  results = results)
me_A   <- list(player_id = 1L, session_id = 1L, market_id = 1L, firm_label = "Firm A")
sub_of <- function(st, pid) { r <- st$submissions[st$submissions$player_id == pid, , drop = FALSE]
                              if (nrow(r)) r else NULL }
res1   <- data.frame(player_id = 1L, computed_at = 999)

test_that("pl_screen_key is stable across all submission activity", {
  none  <- st_open()
  other <- st_open(data.frame(player_id = 2L, price = 42, rationale = "", submitted_at = 111))
  mine  <- st_open(rbind(
    data.frame(player_id = 2L, price = 42, rationale = "", submitted_at = 111),
    data.frame(player_id = 1L, price = 20, rationale = "go low", submitted_at = 222)))

  k0 <- pl_screen_key(none,  me_A, sub_of(none,  1), NULL)
  k1 <- pl_screen_key(other, me_A, sub_of(other, 1), NULL)   # rival submitted
  k2 <- pl_screen_key(mine,  me_A, sub_of(mine,  1), NULL)   # I auto-saved
  expect_identical(k0, k1)
  expect_identical(k0, k2)                                   # my own edits don't churn the screen

  k_next   <- pl_screen_key(st_open(round = 2), me_A, NULL, NULL)
  k_scored <- pl_screen_key(st_open(results = res1), me_A, NULL, res1)
  expect_false(identical(k0, k_next))                        # new round -> re-render
  expect_false(identical(k0, k_scored))                      # results in -> re-render
})

test_that("the reactiveVal gate only re-renders on a real key change", {
  skip_if_not(exists("flushReact", where = asNamespace("shiny")))
  flush <- get("flushReact", asNamespace("shiny"))
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE), add = TRUE)

  state <- shiny::reactiveVal(st_open())
  me    <- shiny::reactive(me_A)
  mysub <- shiny::reactive(sub_of(state(), 1))
  myres <- shiny::reactive(if (nrow(state()$results)) res1 else NULL)

  key <- shiny::reactiveVal("init")
  shiny::observe({
    k <- pl_screen_key(state(), me(), mysub(), myres())
    if (!identical(k, shiny::isolate(key()))) key(k)
  })
  renders <- 0
  shiny::observe({ key(); renders <<- renders + 1 })
  flush(); base <- renders

  state(st_open(data.frame(player_id = 2L, price = 42, rationale = "", submitted_at = 1)))
  flush(); expect_equal(renders, base)                       # rival submitted -> no re-render
  state(st_open(rbind(
    data.frame(player_id = 2L, price = 30, rationale = "", submitted_at = 2),
    data.frame(player_id = 1L, price = 15, rationale = "", submitted_at = 3))))
  flush(); expect_equal(renders, base)                       # I auto-saved -> no re-render
  state(st_open(round = 2))
  flush(); expect_equal(renders, base + 1)                   # next round -> one re-render
  state(st_open(round = 2, results = res1))
  flush(); expect_equal(renders, base + 2)                   # results in -> one re-render
})

test_that("the lobby head-count keeps counting while a student waits", {
  con <- fresh_db()
  sid <- sl_create_session(con, "bertrand", GAMES_PATH)
  join(con, sid, c("Ada", "Boaz"))
  shiny::testServer(player_server, args = list(con = con), {
    session$elapse(1100)
    expect_match(output$lobby_count$html, "2 player\\(s\\) in the lobby")
    join(con, sid, "Cleo")                                   # someone joins later
    session$elapse(1100)
    expect_match(output$lobby_count$html, "3 player\\(s\\) in the lobby")
  })
})
