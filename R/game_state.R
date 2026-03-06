# ============================================
# GAME STATE MANAGEMENT
# ============================================

# This will be created in server()
# Placeholder for reference
# game_state <- reactiveValues(...)

#' Add player to game
#' @param player_name string player name
#' @return player_id or NULL if failed
#' Add player to game
add_player <- function(player_name) {
  
  # Sanitize name
  player_name <- trimws(player_name)
  player_name <- gsub("[^A-Za-z0-9 _-]", "", player_name)
  player_name <- substr(player_name, 1, 20)
  
  if (nchar(player_name) == 0) {
    return(NULL)
  }
  
  # ✅ แก้ตรงนี้ - เช็คชื่อซ้ำแบบ case-insensitive
  for (p in game_state$players) {
    if (tolower(p$name) == tolower(player_name)) {
      # Return existing player ID
      return(p$id)
    }
  }
  
  # Check if full
  if (length(game_state$players) >= game_state$max_players) {
    return(NULL)
  }
  
  player_id <- length(game_state$players) + 1
  
  game_state$players[[player_id]] <- list(
    id = player_id,
    name = player_name,
    chips = 1000,
    cards = character(),
    bet_this_round = 0,
    total_bet_this_hand = 0,
    folded = FALSE,
    all_in = FALSE,
    stats = list(
      hands_played = 0,
      hands_won = 0,
      total_won = 0,
      current_streak = 0,
      biggest_pot = 0
    )
  )
  
  log_action(paste("✅", player_name, "joined the game"))
  
  return(player_id)
}

#' Start new hand
#' @return logical TRUE if started successfully
start_game <- function() {
  
  if (length(game_state$players) < 2) {
    return(FALSE)
  }
  
  # Update stats
  for (i in seq_along(game_state$players)) {
    game_state$players[[i]]$cards <- character()
    game_state$players[[i]]$bet_this_round <- 0
    game_state$players[[i]]$total_bet_this_hand <- 0
    game_state$players[[i]]$folded <- FALSE
    game_state$players[[i]]$all_in <- FALSE
    game_state$players[[i]]$stats$hands_played <- 
      game_state$players[[i]]$stats$hands_played + 1
  }
  
  # Create deck
  game_state$deck <- create_deck()$card_id
  game_state$deck <- sample(game_state$deck)
  
  # Reset state
  game_state$community_cards <- character()
  game_state$pot <- 0
  game_state$side_pots <- list()
  game_state$current_bet <- game_state$big_blind
  game_state$last_raise_amount <- game_state$big_blind
  game_state$current_round <- "preflop"
  game_state$game_started <- TRUE
  game_state$turn_started_at <- NULL
  
  # Post blinds
  post_blinds()
  
  # Deal cards
  deal_hole_cards()
  
  # First to act (UTG - after BB)
  n_players <- length(game_state$players)
  first_to_act <- ((game_state$dealer_position + 2) %% n_players) + 1
  
  start_turn_timer(first_to_act)
  
  log_action("=== NEW HAND ===")
  log_action(paste("🎲 Dealer:", 
                   game_state$players[[game_state$dealer_position]]$name))
  
  return(TRUE)
}

#' Post small and big blinds
post_blinds <- function() {
  n_players <- length(game_state$players)
  
  # Small blind (left of dealer)
  sb_pos <- (game_state$dealer_position %% n_players) + 1
  sb_player <- game_state$players[[sb_pos]]
  sb_amount <- min(game_state$small_blind, sb_player$chips)
  
  game_state$players[[sb_pos]]$bet_this_round <- sb_amount
  game_state$players[[sb_pos]]$total_bet_this_hand <- sb_amount
  game_state$players[[sb_pos]]$chips <- sb_player$chips - sb_amount
  game_state$pot <- game_state$pot + sb_amount
  
  if (sb_amount >= sb_player$chips) {
    game_state$players[[sb_pos]]$all_in <- TRUE
  }
  
  log_action(paste(sb_player$name, "posts SB $", sb_amount))
  
  # Big blind (left of SB)
  bb_pos <- ((game_state$dealer_position + 1) %% n_players) + 1
  bb_player <- game_state$players[[bb_pos]]
  bb_amount <- min(game_state$big_blind, bb_player$chips)
  
  game_state$players[[bb_pos]]$bet_this_round <- bb_amount
  game_state$players[[bb_pos]]$total_bet_this_hand <- bb_amount
  game_state$players[[bb_pos]]$chips <- bb_player$chips - bb_amount
  game_state$pot <- game_state$pot + bb_amount
  
  if (bb_amount >= bb_player$chips) {
    game_state$players[[bb_pos]]$all_in <- TRUE
  }
  
  log_action(paste(bb_player$name, "posts BB $", bb_amount))
}

#' Deal 2 hole cards to each player
deal_hole_cards <- function() {
  for (i in seq_along(game_state$players)) {
    game_state$players[[i]]$cards <- game_state$deck[1:2]
    game_state$deck <- game_state$deck[-(1:2)]
  }
}

#' Deal community cards
#' @param n_cards number of cards to deal
#' @return vector of dealt cards
deal_community <- function(n_cards) {
  # Burn card
  game_state$deck <- game_state$deck[-1]
  
  # Deal
  new_cards <- game_state$deck[1:n_cards]
  game_state$community_cards <- c(game_state$community_cards, new_cards)
  game_state$deck <- game_state$deck[-(1:n_cards)]
  
  return(new_cards)
}

#' Player folds
#' @param player_id ID of player
player_fold <- function(player_id) {
  game_state$players[[player_id]]$folded <- TRUE
  game_state$turn_started_at <- NULL
  
  # Update losing streak
  if (game_state$players[[player_id]]$stats$current_streak <= 0) {
    game_state$players[[player_id]]$stats$current_streak <- 
      game_state$players[[player_id]]$stats$current_streak - 1
  } else {
    game_state$players[[player_id]]$stats$current_streak <- -1
  }
  
  log_action(paste(game_state$players[[player_id]]$name, "folds"))
  
  next_player()
}

#' Player calls or checks
#' @param player_id ID of player
player_call <- function(player_id) {
  player <- game_state$players[[player_id]]
  call_amount <- game_state$current_bet - player$bet_this_round
  
  if (call_amount > player$chips) {
    call_amount <- player$chips
    player$all_in <- TRUE
  }
  
  player$bet_this_round <- player$bet_this_round + call_amount
  player$total_bet_this_hand <- player$total_bet_this_hand + call_amount
  player$chips <- player$chips - call_amount
  game_state$pot <- game_state$pot + call_amount
  
  game_state$players[[player_id]] <- player
  game_state$turn_started_at <- NULL
  
  action <- if (call_amount == 0) "checks" else paste("calls $", call_amount)
  log_action(paste(player$name, action))
  
  next_player()
}

#' Player raises
#' @param player_id ID of player
#' @param raise_amount amount to raise by
#' @return list(success, error)
player_raise <- function(player_id, raise_amount) {
  
  # Validate
  validation <- validate_raise(raise_amount, player_id)
  if (!validation$valid) {
    return(list(success = FALSE, error = validation$error))
  }
  
  player <- game_state$players[[player_id]]
  
  # Calculate
  call_amount <- game_state$current_bet - player$bet_this_round
  total_to_add <- call_amount + raise_amount
  
  if (total_to_add >= player$chips) {
    total_to_add <- player$chips
    player$all_in <- TRUE
  }
  
  player$bet_this_round <- player$bet_this_round + total_to_add
  player$total_bet_this_hand <- player$total_bet_this_hand + total_to_add
  player$chips <- player$chips - total_to_add
  game_state$pot <- game_state$pot + total_to_add
  
  # Update state
  game_state$current_bet <- player$bet_this_round
  game_state$last_raise_amount <- raise_amount
  game_state$players[[player_id]] <- player
  game_state$turn_started_at <- NULL
  
  log_action(paste(player$name, "raises to $", player$bet_this_round))
  
  next_player()
  
  return(list(success = TRUE, error = NULL))
}

#' Move to next active player
#' Move to next active player
next_player <- function() {
  
  n_players <- length(game_state$players)
  
  # Count players who still need to act
  need_to_act <- 0
  for (p in game_state$players) {
    if (!p$folded && !p$all_in && p$bet_this_round < game_state$current_bet) {
      need_to_act <- need_to_act + 1
    }
  }
  
  # If no one needs to act, end betting round
  if (need_to_act == 0) {
    end_betting_round()
    return()
  }
  
  # Find next player who needs to act
  attempts <- 0
  
  repeat {
    game_state$current_turn <- (game_state$current_turn %% n_players) + 1
    attempts <- attempts + 1
    
    if (attempts > n_players) {
      end_betting_round()
      return()
    }
    
    player <- game_state$players[[game_state$current_turn]]
    
    if (!player$folded && !player$all_in && 
        player$bet_this_round < game_state$current_bet) {
      start_turn_timer(game_state$current_turn)
      return()
    }
  }
}

#' End betting round and move to next
end_betting_round <- function() {
  
  # Reset round bets
  for (i in seq_along(game_state$players)) {
    game_state$players[[i]]$bet_this_round <- 0
  }
  
  game_state$current_bet <- 0
  game_state$last_raise_amount <- 0
  game_state$turn_started_at <- NULL
  
  # Next stage
  if (game_state$current_round == "preflop") {
    game_state$current_round <- "flop"
    cards <- deal_community(3)
    log_action(paste("=== FLOP:", paste(cards, collapse = " "), "==="))
    
  } else if (game_state$current_round == "flop") {
    game_state$current_round <- "turn"
    cards <- deal_community(1)
    log_action(paste("=== TURN:", cards, "==="))
    
  } else if (game_state$current_round == "turn") {
    game_state$current_round <- "river"
    cards <- deal_community(1)
    log_action(paste("=== RIVER:", cards, "==="))
    
  } else {
    showdown()
    return()
  }
  
  # First to act (left of dealer)
  n_players <- length(game_state$players)
  first <- (game_state$dealer_position %% n_players) + 1
  
  for (i in 0:(n_players-1)) {
    pos <- ((first + i - 1) %% n_players) + 1
    if (!game_state$players[[pos]]$folded && 
        !game_state$players[[pos]]$all_in) {
      start_turn_timer(pos)
      return()
    }
  }
  
  showdown()
}

#' Showdown - determine winners
showdown <- function() {
  game_state$current_round <- "showdown"
  game_state$turn_started_at <- NULL
  
  log_action("=== SHOWDOWN ===")
  
  # Show hands
  for (p in game_state$players) {
    if (!p$folded) {
      log_action(paste(p$name, ":", paste(p$cards, collapse = " ")))
    }
  }
  
  # Calculate and distribute pots
  pots <- calculate_side_pots(game_state$players)
  game_state$players <- distribute_pots(pots, game_state$players,
                                        game_state$community_cards)
  
  # Move dealer
  game_state$dealer_position <- (game_state$dealer_position %% 
                                   length(game_state$players)) + 1
  
  # Reset
  Sys.sleep(3)
  game_state$game_started <- FALSE
  game_state$current_round <- "waiting"
  game_state$pot <- 0
}

#' Log action to game log
#' @param message string message
log_action <- function(message) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  game_state$action_log <- c(
    paste0("[", timestamp, "] ", message),
    game_state$action_log
  )
  
  if (length(game_state$action_log) > 50) {
    game_state$action_log <- game_state$action_log[1:50]
  }
}