# ============================================================
# WIN PROBABILITY  –  Monte Carlo simulation
# ============================================================

# Estimate win probability for a player given their hole cards,
# current community cards, and number of active opponents.
#   hole_cards      : character vector of length 2 (e.g. c("As","Kh"))
#   community_cards : character vector, 0–5 cards
#   n_opponents     : integer >= 1
#   n_sim           : number of simulations (default 400)
# Returns a list:
#   $win_pct   – estimated win % (0–100)
#   $tie_pct   – estimated tie %
#   $hand_dist – data.frame with hand type distribution across sims
calc_win_prob <- function(hole_cards, community_cards, n_opponents, n_sim = 400) {

  if (length(hole_cards) != 2 || n_opponents < 1) {
    return(list(win_pct = NA, tie_pct = NA, hand_dist = NULL))
  }

  deck    <- create_deck()
  known   <- c(hole_cards, community_cards)
  remain  <- setdiff(deck, known)
  n_board <- 5L - length(community_cards)

  # Need enough cards for board + all opponents
  if (length(remain) < n_board + n_opponents * 2L) {
    n_sim <- min(n_sim, 100L)
  }

  wins  <- 0L
  ties  <- 0L
  hand_counts <- integer(10L)  # index 1 = High Card ... 10 = Royal Flush

  for (s in seq_len(n_sim)) {
    shuf  <- sample(remain)
    board <- c(community_cards, shuf[seq_len(n_board)])
    pool  <- shuf[seq(n_board + 1L, length(shuf))]

    my_cards <- c(hole_cards, board)
    my_hand  <- evaluate_hand(my_cards)

    # Track hand distribution
    hi <- my_hand$rank + 1L
    if (hi >= 1L && hi <= 10L) hand_counts[hi] <- hand_counts[hi] + 1L

    # Compare against opponents
    result <- 1L   # 1=win, 0=tie, -1=lose
    for (opp in seq_len(n_opponents)) {
      idx <- (opp - 1L) * 2L + 1L
      if (idx + 1L > length(pool)) { result <- 0L; break }
      opp_cards <- c(pool[idx], pool[idx + 1L], board)
      cmp <- compare_hands(my_cards, opp_cards)
      if (cmp < 0L) { result <- -1L; break }
      if (cmp == 0L) result <- 0L
    }
    if (result == 1L) wins <- wins + 1L
    else if (result == 0L) ties <- ties + 1L
  }

  hand_dist <- data.frame(
    Hand        = HAND_NAMES,
    Probability = paste0(round(hand_counts / n_sim * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  # Only show rows with nonzero prob to keep table short
  hand_dist <- hand_dist[hand_counts > 0, ]

  list(
    win_pct   = round(wins / n_sim * 100, 1),
    tie_pct   = round(ties / n_sim * 100, 1),
    hand_dist = hand_dist
  )
}
