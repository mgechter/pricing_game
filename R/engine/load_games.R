# Load the declarative game definitions.

load_games <- function(path = "config/games.yaml") {
  cfg   <- yaml::read_yaml(path)
  games <- cfg$games %||% cfg
  ids   <- vapply(games, function(g) g$id %||% NA_character_, character(1))
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("every game in ", path, " needs a unique `id`", call. = FALSE)
  }
  stats::setNames(games, ids)
}

get_game <- function(id, path = "config/games.yaml") {
  g <- load_games(path)[[id]]
  if (is.null(g)) stop("no game with id: ", id, call. = FALSE)
  g
}

# Fill in benchmark prices. Any entry that is missing or "compute" is derived:
#   ne         numerically (fixed point of the best response)   -- model-agnostic
#   collusive  numerically (argmax of joint profit)             -- model-agnostic
#   monopoly   from the closed form  (the textbook reference the slides draw)
# `method = "closed"` uses the hand-derived formulas for ne / collusive too.
# An explicit number in games.yaml is always kept as-is.
resolve_benchmarks <- function(game, method = c("numeric", "closed")) {
  method <- match.arg(method)
  model  <- game$demand$model
  params <- game$demand$params
  b <- game$benchmarks %||% list()

  todo <- function(x) is.null(x) || identical(x, "compute")
  # Snap a numeric solve back onto the closed form when they agree to <= $0.50 —
  # keeps benchmark/reference-line values slide-clean (100, not 100.001).
  snap <- function(num, closed) {
    if (length(closed) != 1 || is.na(closed)) return(num)
    if (abs(num - closed) <= 0.5) closed else num
  }
  ne_val <- function() if (method == "numeric")
    snap(ne_price_numeric(model, params), tryCatch(ne_price(model, params), error = function(e) NA))
    else ne_price(model, params)
  co_val <- function() if (method == "numeric")
    snap(collusive_price_numeric(model, params),
         tryCatch(collusive_price(model, params), error = function(e) NA))
    else collusive_price(model, params)

  if (todo(b$ne))        b$ne        <- ne_val()
  if (todo(b$monopoly))  b$monopoly  <- monopoly_price(model, params)
  if (todo(b$collusive)) b$collusive <- co_val()

  game$benchmarks <- b
  game
}
