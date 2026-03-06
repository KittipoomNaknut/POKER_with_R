# ============================================================
# BETTING  –  side pots, pot distribution, raise validation
# ============================================================

calculate_side_pots <- function(players) {
  active <- Filter(function(p) !isTRUE(p$folded), players)
  if (length(active) == 0L) return(list())
  
  all_bets    <- vapply(players, function(p) p$total_bet_this_hand, numeric(1))
  unique_bets <- sort(unique(all_bets[all_bets > 0]))
  if (length(unique_bets) == 0L) return(list())
  
  pots <- list()
  prev <- 0
  
  for (level in unique_bets) {
    eligible     <- Filter(function(p) !isTRUE(p$folded) &&
                             p$total_bet_this_hand >= level, players)
    contributors <- Filter(function(p) p$total_bet_this_hand >= level, players)
    amount       <- (level - prev) * length(contributors)
    if (amount > 0 && length(eligible) > 0) {
      pots[[length(pots) + 1L]] <- list(
        amount       = amount,
        eligible_ids = vapply(eligible, function(p) p$id, integer(1)),
        bet_level    = level
      )
    }
    prev <- level
  }
  return(pots)
}

distribute_pots <- function(pots, players, community_cards) {
  if (length(pots) == 0L) return(players)
  
  for (pot in pots) {
    elig <- Filter(function(p) p$id %in% pot$eligible_ids, players)
    if (length(elig) == 0L) next
    
    if (length(elig) == 1L) {
      idx <- which(vapply(players, function(p) p$id == elig[[1L]]$id, logical(1)))
      players[[idx]]$chips             <- players[[idx]]$chips + pot$amount
      players[[idx]]$stats$total_won   <- players[[idx]]$stats$total_won + pot$amount
      players[[idx]]$stats$hands_won   <- players[[idx]]$stats$hands_won + 1L
      players[[idx]]$stats$biggest_pot <- max(players[[idx]]$stats$biggest_pot, pot$amount)
      # FIX: streak logic ชัดเจนขึ้น
      players[[idx]]$stats$current_streak <-
        max(1L, players[[idx]]$stats$current_streak + 1L)
      log_action(paste(elig[[1L]]$name, "wins $", round(pot$amount)))
      next
    }
    
    all_cards <- lapply(elig, function(p) c(p$cards, community_cards))
    n         <- length(elig)
    is_best   <- rep(TRUE, n)
    
    for (i in seq_len(n)) {
      if (!is_best[i]) next
      for (j in seq_len(n)) {
        if (i == j || !is_best[j]) next
        cmp <- compare_hands(all_cards[[i]], all_cards[[j]])
        if (cmp < 0) { is_best[i] <- FALSE; break }
        if (cmp > 0)   is_best[j] <- FALSE
      }
    }
    
    winners <- elig[is_best]
    share   <- pot$amount / length(winners)
    
    for (w in winners) {
      idx <- which(vapply(players, function(p) p$id == w$id, logical(1)))
      hand_name <- evaluate_hand(c(w$cards, community_cards))$name
      players[[idx]]$chips             <- players[[idx]]$chips + share
      players[[idx]]$stats$total_won   <- players[[idx]]$stats$total_won + share
      players[[idx]]$stats$hands_won   <- players[[idx]]$stats$hands_won + 1L
      players[[idx]]$stats$biggest_pot <- max(players[[idx]]$stats$biggest_pot, share)
      players[[idx]]$stats$current_streak <-
        max(1L, players[[idx]]$stats$current_streak + 1L)
      log_action(paste(w$name, "wins $", round(share), "with", hand_name))
    }
  }
  
  return(players)
}

validate_raise <- function(raise_amount, player_id) {
  if (player_id > length(game_state$players))
    return(list(valid = FALSE, error = "Invalid player"))
  
  # FIX: reject non-positive raise
  if (is.null(raise_amount) || is.na(raise_amount) || raise_amount <= 0)
    return(list(valid = FALSE, error = "Raise amount must be positive"))
  
  p           <- game_state$players[[player_id]]
  call_amount <- game_state$current_bet - p$bet_this_round
  total       <- call_amount + raise_amount
  
  if (total > p$chips)
    return(list(valid = FALSE,
                error = paste0("Need $", total, " but only have $", p$chips)))
  
  min_r    <- get_min_raise()
  is_allin <- (total >= p$chips)
  
  if (!is_allin && raise_amount < min_r)
    return(list(valid = FALSE,
                error = paste("Minimum raise is $", min_r)))
  
  return(list(valid = TRUE, error = NULL))
}

get_min_raise <- function() {
  max(game_state$big_blind, game_state$last_raise_amount)
}