# ============================================
# BETTING LOGIC - Side Pots & Validation
# ============================================

#' Calculate side pots from current player bets
#' @param players list of player objects
#' @return list of pot objects
calculate_side_pots <- function(players) {
  
  # Get non-folded players
  active <- Filter(function(p) !p$folded, players)
  
  if (length(active) == 0) {
    return(list())
  }
  
  # Get all bet amounts
  bets <- sapply(active, function(p) p$total_bet_this_hand)
  unique_bets <- sort(unique(bets[bets > 0]))
  
  if (length(unique_bets) == 0) {
    return(list())
  }
  
  pots <- list()
  previous_level <- 0
  
  for (level in unique_bets) {
    
    # Who can win this pot? (bet >= level)
    eligible <- Filter(function(p) p$total_bet_this_hand >= level, active)
    
    # Amount in this pot
    contribution <- level - previous_level
    amount <- contribution * length(eligible)
    
    if (amount > 0) {
      pots[[length(pots) + 1]] <- list(
        amount = amount,
        eligible_players = sapply(eligible, function(p) p$id),
        bet_level = level
      )
    }
    
    previous_level <- level
  }
  
  return(pots)
}

#' Distribute pots to winners
#' @param pots list of pot objects from calculate_side_pots()
#' @param players list of player objects
#' @param community_cards community cards
#' @return updated players list with chips distributed
distribute_pots <- function(pots, players, community_cards) {
  
  if (length(pots) == 0) {
    return(players)
  }
  
  for (pot_idx in seq_along(pots)) {
    pot <- pots[[pot_idx]]
    
    # Get eligible non-folded players
    eligible_players <- Filter(
      function(p) p$id %in% pot$eligible_players && !p$folded,
      players
    )
    
    if (length(eligible_players) == 0) {
      next
    }
    
    # If only one player left, they win
    if (length(eligible_players) == 1) {
      winner <- eligible_players[[1]]
      winner_idx <- which(sapply(players, function(p) p$id == winner$id))
      
      players[[winner_idx]]$chips <- players[[winner_idx]]$chips + pot$amount
      players[[winner_idx]]$stats$total_won <- 
        players[[winner_idx]]$stats$total_won + pot$amount
      players[[winner_idx]]$stats$hands_won <- 
        players[[winner_idx]]$stats$hands_won + 1
      
      if (pot$amount > players[[winner_idx]]$stats$biggest_pot) {
        players[[winner_idx]]$stats$biggest_pot <- pot$amount
      }
      
      log_action(paste(winner$name, "wins $", round(pot$amount, 2), 
                       "(Pot", pot_idx, "- others folded)"))
      next
    }
    
    # Evaluate all hands
    hands <- lapply(eligible_players, function(p) {
      all_cards <- c(p$cards, community_cards)
      hand_eval <- evaluate_hand(all_cards)
      list(player = p, hand = hand_eval)
    })
    
    # Find best hand(s)
    best_rank <- max(sapply(hands, function(h) h$hand$rank))
    winners <- Filter(function(h) h$hand$rank == best_rank, hands)
    
    # Split pot
    share <- pot$amount / length(winners)
    
    for (winner_info in winners) {
      winner <- winner_info$player
      winner_idx <- which(sapply(players, function(p) p$id == winner$id))
      
      players[[winner_idx]]$chips <- players[[winner_idx]]$chips + share
      players[[winner_idx]]$stats$total_won <- 
        players[[winner_idx]]$stats$total_won + share
      players[[winner_idx]]$stats$hands_won <- 
        players[[winner_idx]]$stats$hands_won + 1
      
      if (share > players[[winner_idx]]$stats$biggest_pot) {
        players[[winner_idx]]$stats$biggest_pot <- share
      }
      
      # Update streak
      if (players[[winner_idx]]$stats$current_streak >= 0) {
        players[[winner_idx]]$stats$current_streak <- 
          players[[winner_idx]]$stats$current_streak + 1
      } else {
        players[[winner_idx]]$stats$current_streak <- 1
      }
      
      log_action(paste(winner$name, "wins $", round(share, 2),
                       "with", winner_info$hand$hand,
                       "(Pot", pot_idx, ")"))
    }
  }
  
  return(players)
}

#' Validate raise amount
#' @param raise_amount amount to raise
#' @param player_id player making raise
#' @return list(valid, error)
validate_raise <- function(raise_amount, player_id) {
  
  if (!exists("game_state")) {
    return(list(valid = FALSE, error = "Game state not found"))
  }
  
  if (player_id > length(game_state$players)) {
    return(list(valid = FALSE, error = "Invalid player"))
  }
  
  player <- game_state$players[[player_id]]
  
  # Check chips available
  call_amount <- game_state$current_bet - player$bet_this_round
  total_needed <- call_amount + raise_amount
  
  if (total_needed > player$chips) {
    return(list(
      valid = FALSE,
      error = paste("Need $", total_needed, "but only have $", player$chips)
    ))
  }
  
  # Check minimum raise
  min_raise <- get_min_raise()
  
  if (raise_amount < min_raise) {
    return(list(
      valid = FALSE,
      error = paste("Minimum raise is $", min_raise)
    ))
  }
  
  return(list(valid = TRUE, error = NULL))
}

#' Get minimum raise amount
#' @return numeric minimum raise
get_min_raise <- function() {
  if (!exists("game_state")) {
    return(10)
  }
  
  max(game_state$big_blind, game_state$last_raise_amount)
}