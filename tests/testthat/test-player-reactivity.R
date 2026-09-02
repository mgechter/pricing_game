# The student's price entry must survive another student submitting. That means
# pl_screen_key() must be stable to everything except this student's own view,
# and the screen must be gated through a reactiveVal (value-diffed), not a plain
# reactive() (which re-invalidates downstream even when unchanged).

suppressMessages(library(shiny))
src <- function(f) source(file.path(normalizePath(file.path(getwd(), "..", "..")), "R", f))
src("ui_helpers.R"); src("mod_player.R")

st_open <- function(subs = NULL, round = 1) list(
  session = list(phase = "open", current_round = round, session_id = 1),
  submissions = subs %||% data.frame(player_id = integer(), price = numeric(),
                                     rationale = character(), submitted_at = numeric()),
  results = data.frame())
me_A   <- list(player_id = 1L, session_id = 1L, market_id = 1L, firm_label = "Firm A")
sub_of <- function(st, pid) { r <- st$submissions[st$submissions$player_id == pid, , drop = FALSE]
                              if (nrow(r)) r else NULL }

test_that("pl_screen_key ignores another student's submission but tracks my own", {
  none  <- st_open()
  other <- st_open(data.frame(player_id = 2L, price = 42, rationale = "", submitted_at = 111))
  mine  <- st_open(rbind(
    data.frame(player_id = 2L, price = 42, rationale = "", submitted_at = 111),
    data.frame(player_id = 1L, price = 20, rationale = "go low", submitted_at = 222)))

  k0 <- pl_screen_key(none,  me_A, sub_of(none,  1), NULL)
  k1 <- pl_screen_key(other, me_A, sub_of(other, 1), NULL)   # rival submitted
  k2 <- pl_screen_key(mine,  me_A, sub_of(mine,  1), NULL)   # I submitted
  k3 <- pl_screen_key(st_open(round = 2), me_A, NULL, NULL)  # next round

  expect_identical(k0, k1)      # a rival's submission must not change my key
  expect_false(identical(k1, k2))
  expect_false(identical(k2, k3))
})

test_that("the reactiveVal gate only re-renders on a real key change", {
  skip_if_not(exists("flushReact", where = asNamespace("shiny")))
  flush <- get("flushReact", asNamespace("shiny"))
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE), add = TRUE)

  state <- shiny::reactiveVal(st_open())
  me    <- shiny::reactive(me_A)
  mysub <- shiny::reactive(sub_of(state(), 1))
  myres <- shiny::reactive(NULL)

  key <- shiny::reactiveVal("init")
  shiny::observe({
    k <- pl_screen_key(state(), me(), mysub(), myres())
    if (!identical(k, shiny::isolate(key()))) key(k)
  })
  renders <- 0
  shiny::observe({ key(); renders <<- renders + 1 })
  flush(); base <- renders

  state(st_open(data.frame(player_id = 2L, price = 42, rationale = "", submitted_at = 1)))
  flush(); expect_equal(renders, base)                     # rival submitted → no re-render
  state(st_open(data.frame(player_id = 2L, price = 30, rationale = "", submitted_at = 2)))
  flush(); expect_equal(renders, base)                     # rival edited → no re-render
  state(st_open(rbind(
    data.frame(player_id = 2L, price = 30, rationale = "", submitted_at = 2),
    data.frame(player_id = 1L, price = 15, rationale = "", submitted_at = 3))))
  flush(); expect_equal(renders, base + 1)                 # I submitted → one re-render
  state(st_open(round = 2))
  flush(); expect_equal(renders, base + 2)                 # next round → one re-render
})
