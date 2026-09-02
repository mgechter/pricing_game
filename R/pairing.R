# Assign players to 2-firm markets.
#
# v1: random pairing at game start, fixed for the game. Odd headcount -> the last
# player (after shuffle) sits the game out. (Phase 2 adds a bot / instructor firm
# and rotation-aware pairing across games.)

make_pairs <- function(player_ids, seed = NULL) {
  stopifnot(length(player_ids) >= 2)
  if (!is.null(seed)) set.seed(seed)

  ids <- sample(player_ids)
  n_paired    <- length(ids) - (length(ids) %% 2)
  sitting_out <- if (n_paired < length(ids)) ids[length(ids)] else ids[0]

  m <- matrix(ids[seq_len(n_paired)], ncol = 2, byrow = TRUE)
  pairs <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
    data.frame(player_id   = m[i, ],
               market_id    = i,
               firm_label   = c("Firm A", "Firm B"),
               stringsAsFactors = FALSE)
  }))

  list(pairs = pairs, sitting_out = sitting_out)
}
