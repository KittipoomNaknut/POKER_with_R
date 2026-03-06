# ============================================================
# GAME STATE MANAGEMENT
# game_state is a reactiveValues object created at app startup
# ============================================================

# ---- Player management ----

add_player <- function(player_name) {
  player_name <- trimws(player_name)
  player_name <- gsub("[^A-Za-z0-9 _-]", "", player_name)
  player_name <- substr(player_name, 1, 20)
  if (nchar(player_name) == 0) return(NULL)

  # Ensure unique name
  base  <- player_name
  cnt   <- 2
  repeat {
    exists <- any(vapply(game_state$players,
                         function(p) tolower(p$name) == tolower(player_name),
                         logical(1)))
    if (!exists) break
    player_name <- paste0(base, cnt); cnt <- cnt + 1
    if (cnt > 99) return(NULL)
  }

  if (length(game_state$players) >= game_state$max_players) return(NULL)

  pid <- length(game_state$players) + 1L
  game_state$players[[pid]] <- list(
    id                 = pid,
    name               = player_name,
    chips              = game_state$starting_chips,
    cards              = character(),
    bet_this_round     = 0,
    total_bet_this_hand = 0,
    folded             = FALSE,
    all_in             = FALSE,
    stats = list(
      hands_played    = 0L,
      hands_won       = 0L,
      total_won       = 0,
      biggest_pot     = 0,
      current_streak  = 0L
    )
  )
  log_action(paste(player_name, "joined"))
  return(pid)
}

# ---- New hand ----

start_game <- function() {
  n <- length(game_state$players)
  if (n < 2) return(FALSE)

  # Reset each player for new hand
  for (i in seq_len(n)) {
    game_state$players[[i]]$cards              <- character()
    game_state$players[[i]]$bet_this_round     <- 0
    game_state$players[[i]]$total_bet_this_hand <- 0
    game_state$players[[i]]$folded             <- FALSE
    game_state$players[[i]]$all_in             <- FALSE
    game_state$players[[i]]$stats$hands_played <-
      game_state$players[[i]]$stats$hands_played + 1L
  }

  game_state$deck            <- sample(create_deck())
  game_state$community_cards <- character()
  game_state$pot             <- 0
  game_state$current_bet     <- game_state$big_blind
  game_state$last_raise_amount <- game_state$big_blind
  game_state$current_round   <- "preflop"
  game_state$game_started    <- TRUE
  game_state$turn_started_at <- NULL
  game_state$showdown_at     <- NULL

  post_blinds()
  deal_hole_cards()

  # UTG = dealer + 3 (heads-up: dealer acts first preflop)
  first <- if (n == 2L) game_state$dealer_position else
    ((game_state$dealer_position + 2L) %% n) + 1L

  # Skip busted players (0 chips, all_in from blinds)
  for (offset in seq_len(n)) {
    pos <- ((first - 1L + offset - 1L) %% n) + 1L
    if (!game_state$players[[pos]]$all_in && !game_state$players[[pos]]$folded) {
      start_turn_timer(pos)
      break
    }
  }

  log_action(paste("=== NEW HAND === Dealer:",
                   game_state$players[[game_state$dealer_position]]$name))
  return(TRUE)
}

# ---- Blinds ----

post_blinds <- function() {
  n   <- length(game_state$players)
  dp  <- game_state$dealer_position
  sb  <- (dp %% n) + 1L
  bb  <- ((dp + 1L) %% n) + 1L

  .post <- function(pos, amount_full, label) {
    p      <- game_state$players[[pos]]
    amount <- min(amount_full, p$chips)
    game_state$players[[pos]]$bet_this_round      <- amount
    game_state$players[[pos]]$total_bet_this_hand <- amount
    game_state$players[[pos]]$chips               <- p$chips - amount
    game_state$pot                                <- game_state$pot + amount
    if (p$chips - amount <= 0) game_state$players[[pos]]$all_in <- TRUE
    log_action(paste(p$name, label, "$", amount))
  }

  .post(sb, game_state$small_blind, "posts SB")
  .post(bb, game_state$big_blind,   "posts BB")
}

# ---- Deal cards ----

deal_hole_cards <- function() {
  for (i in seq_along(game_state$players)) {
    game_state$players[[i]]$cards <- game_state$deck[1:2]
    game_state$deck <- game_state$deck[-(1:2)]
  }
}

deal_community <- function(n_cards) {
  game_state$deck        <- game_state$deck[-1]        # burn
  cards                  <- game_state$deck[1:n_cards]
  game_state$community_cards <- c(game_state$community_cards, cards)
  game_state$deck        <- game_state$deck[-(1:n_cards)]
  return(cards)
}

# ---- Player actions ----

player_fold <- function(player_id) {
  p <- game_state$players[[player_id]]
  game_state$players[[player_id]]$folded <- TRUE
  game_state$turn_started_at <- NULL
  # Update loss streak
  s <- p$stats$current_streak
  game_state$players[[player_id]]$stats$current_streak <-
    if (s <= 0L) s - 1L else -1L
  log_action(paste(p$name, "folds"))
  next_player()
}

player_call <- function(player_id) {
  p           <- game_state$players[[player_id]]
  call_amount <- game_state$current_bet - p$bet_this_round
  if (call_amount >= p$chips) {
    call_amount <- p$chips
    game_state$players[[player_id]]$all_in <- TRUE
  }
  game_state$players[[player_id]]$bet_this_round      <- p$bet_this_round + call_amount
  game_state$players[[player_id]]$total_bet_this_hand <- p$total_bet_this_hand + call_amount
  game_state$players[[player_id]]$chips               <- p$chips - call_amount
  game_state$pot             <- game_state$pot + call_amount
  game_state$turn_started_at <- NULL
  action <- if (call_amount == 0) "checks" else paste("calls $", call_amount)
  log_action(paste(p$name, action))
  next_player()
}

player_raise <- function(player_id, raise_amount) {
  v <- validate_raise(raise_amount, player_id)
  if (!v$valid) return(list(success = FALSE, error = v$error))

  p           <- game_state$players[[player_id]]
  call_amount <- game_state$current_bet - p$bet_this_round
  total       <- min(call_amount + raise_amount, p$chips)

  if (total >= p$chips) game_state$players[[player_id]]$all_in <- TRUE
  game_state$players[[player_id]]$bet_this_round      <- p$bet_this_round + total
  game_state$players[[player_id]]$total_bet_this_hand <- p$total_bet_this_hand + total
  game_state$players[[player_id]]$chips               <- p$chips - total
  game_state$pot              <- game_state$pot + total
  game_state$current_bet      <- p$bet_this_round + total
  game_state$last_raise_amount <- raise_amount
  game_state$turn_started_at  <- NULL

  log_action(paste(p$name, "raises to $", game_state$current_bet))
  next_player()
  return(list(success = TRUE, error = NULL))
}

# ---- Turn advancement ----

next_player <- function() {
  n <- length(game_state$players)

  # Check if only one non-folded player
  active_count <- sum(vapply(game_state$players, function(p) !p$folded, logical(1)))
  if (active_count <= 1L) { end_betting_round(); return() }

  # Players who still need to match current_bet
  need_act <- sum(vapply(game_state$players, function(p)
    !p$folded && !p$all_in && p$bet_this_round < game_state$current_bet, logical(1)))

  if (need_act == 0L) { end_betting_round(); return() }

  # Advance to next player who needs to act
  for (step in seq_len(n)) {
    game_state$current_turn <- (game_state$current_turn %% n) + 1L
    p <- game_state$players[[game_state$current_turn]]
    if (!p$folded && !p$all_in && p$bet_this_round < game_state$current_bet) {
      start_turn_timer(game_state$current_turn)
      return()
    }
  }
  end_betting_round()
}

end_betting_round <- function() {
  n <- length(game_state$players)

  # If only 1 left, immediate showdown
  active <- sum(vapply(game_state$players, function(p) !p$folded, logical(1)))
  if (active <= 1L) { showdown(); return() }

  # Reset round bets
  for (i in seq_len(n)) game_state$players[[i]]$bet_this_round <- 0
  game_state$current_bet      <- 0
  game_state$last_raise_amount <- 0
  game_state$turn_started_at  <- NULL

  # Advance round
  if (game_state$current_round == "preflop") {
    game_state$current_round <- "flop"
    cards <- deal_community(3)
    log_action(paste("FLOP:", format_cards(cards)))
  } else if (game_state$current_round == "flop") {
    game_state$current_round <- "turn"
    log_action(paste("TURN:", format_card(deal_community(1))))
  } else if (game_state$current_round == "turn") {
    game_state$current_round <- "river"
    log_action(paste("RIVER:", format_card(deal_community(1))))
  } else {
    showdown(); return()
  }

  # Find first to act (left of dealer)
  first <- (game_state$dealer_position %% n) + 1L
  for (offset in seq_len(n)) {
    pos <- ((first + offset - 2L) %% n) + 1L
    if (!game_state$players[[pos]]$folded && !game_state$players[[pos]]$all_in) {
      start_turn_timer(pos)
      return()
    }
  }
  showdown()
}

# ---- Showdown ----

showdown <- function() {
  game_state$current_round   <- "showdown"
  game_state$turn_started_at <- NULL
  log_action("=== SHOWDOWN ===")

  for (p in game_state$players) {
    if (!p$folded && length(p$cards) > 0) {
      hand_result <- evaluate_hand(c(p$cards, game_state$community_cards))
      log_action(paste(p$name, ":", format_cards(p$cards), "->", hand_result$name))
    }
  }

  pots <- calculate_side_pots(game_state$players)
  game_state$players <- distribute_pots(pots, game_state$players,
                                        game_state$community_cards)

  game_state$dealer_position <-
    (game_state$dealer_position %% length(game_state$players)) + 1L

  # Mark time so server observer can reset after ~3s (no blocking sleep)
  game_state$showdown_at <- Sys.time()
}

# ---- Timer (in game_state to avoid timer.R dependency) ----

start_turn_timer <- function(player_id) {
  game_state$current_turn    <- player_id
  game_state$turn_started_at <- Sys.time()
}

get_time_remaining <- function() {
  if (is.null(game_state$turn_started_at)) return(game_state$turn_timer_seconds)
  elapsed   <- as.numeric(difftime(Sys.time(), game_state$turn_started_at, units="secs"))
  remaining <- game_state$turn_timer_seconds - elapsed
  max(0, round(remaining))
}

check_and_handle_timeout <- function() {
  if (!game_state$game_started) return()
  if (is.null(game_state$turn_started_at)) return()
  elapsed <- as.numeric(difftime(Sys.time(), game_state$turn_started_at, units="secs"))
  if (elapsed < game_state$turn_timer_seconds) return()

  pid <- game_state$current_turn
  if (pid < 1L || pid > length(game_state$players)) return()
  p   <- game_state$players[[pid]]
  if (!p$folded && !p$all_in) {
    log_action(paste(p$name, "timed out - auto fold"))
    player_fold(pid)
  }
}

# ---- Utility ----

log_action <- function(msg) {
  ts  <- format(Sys.time(), "%H:%M:%S")
  new <- paste0("[", ts, "] ", msg)
  game_state$action_log <- c(new, game_state$action_log)[1:min(60L,
    length(game_state$action_log) + 1L)]
}
