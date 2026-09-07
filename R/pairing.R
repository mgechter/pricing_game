# Assign players to 2-firm markets.
#
# v1: random pairing at game start, fixed for the game. Odd headcount -> the last
# player (after shuffle) joins an existing market as a *shadow*: they submit a
# price and are scored 1-on-1 against their host's real prices, but the host is
# not scored twice. Nobody sits out.
# (Phase 4 adds rotation-aware pairing across a class's games.)

make_pairs <- function(player_ids, seed = NULL) {
  stopifnot(length(player_ids) >= 2)
  if (!is.null(seed)) set.seed(seed)

  ids <- sample(player_ids)
  n   <- length(ids)
  odd <- (n %% 2 == 1) && n >= 3
  shadow_id <- if (odd) ids[n] else NULL
  paired    <- if (odd) ids[-n] else ids

  m <- matrix(paired, ncol = 2, byrow = TRUE)
  pairs <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
    data.frame(player_id  = m[i, ],
               market_id   = i,
               firm_label  = c("Firm A", "Firm B"),
               shadow_of   = NA_integer_,
               stringsAsFactors = FALSE)
  }))

  shadow <- NULL
  if (odd) {
    host <- sample(paired, 1)
    shadow <- data.frame(
      player_id  = shadow_id,
      market_id  = pairs$market_id[pairs$player_id == host],
      firm_label = "Shadow",
      shadow_of  = host,
      stringsAsFactors = FALSE)
    pairs <- rbind(pairs, shadow)
  }

  list(pairs = pairs, shadow = shadow)
}
