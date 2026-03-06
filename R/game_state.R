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
  
  base <- player_name
  cnt  <- 2
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
  
  # ถ้า join ระหว่าง hand กำลังดำเนินอยู่ ให้ sit_out = TRUE
  # จะถูก reset เป็น FALSE ตอน start_game() ครั้งถัดไป
  joining_mid_hand <- isTRUE(game_state$game_started)
  
  game_state$players[[pid]] <- list(
    id                  = pid,
    name                = player_name,
    chips               = game_state$starting_chips,
    cards               = character(),
    bet_this_round      = 0,
    total_bet_this_hand = 0,
    folded              = joining_mid_hand,   # sit-out this hand
    all_in              = FALSE,
    acted               = FALSE,
    sit_out             = joining_mid_hand,   # flag สำหรับ hand ถัดไป
    stats = list(
      hands_played   = 0L,
      hands_won      = 0L,
      total_won      = 0,
      biggest_pot    = 0,
      current_streak = 0L
    )
  )
  
  if (joining_mid_hand) {
    log_action(paste(player_name, "joined – will play next hand"))
  } else {
    log_action(paste(player_name, "joined"))
  }
  return(pid)
}

# ---- New hand ----

start_game <- function() {
  n <- length(game_state$players)
  if (n < 2) return(FALSE)
  
  # ต้องมี player ที่ไม่ sit_out อย่างน้อย 2 คน
  active_count <- sum(vapply(game_state$players,
                             function(p) !isTRUE(p$sit_out), logical(1)))
  if (active_count < 2) return(FALSE)
  
  for (i in seq_len(n)) {
    game_state$players[[i]]$cards               <- character()
    game_state$players[[i]]$bet_this_round      <- 0
    game_state$players[[i]]$total_bet_this_hand <- 0
    game_state$players[[i]]$all_in              <- FALSE
    game_state$players[[i]]$acted               <- FALSE
    # reset sit_out: hand ถัดไปเล่นได้แล้ว
    was_sitting_out <- isTRUE(game_state$players[[i]]$sit_out)
    game_state$players[[i]]$sit_out <- FALSE
    if (was_sitting_out) {
      # mid-hand joiners: folded this hand, ไม่นับ hands_played
      game_state$players[[i]]$folded <- TRUE
    } else {
      game_state$players[[i]]$folded <- FALSE
      game_state$players[[i]]$stats$hands_played <-
        game_state$players[[i]]$stats$hands_played + 1L
    }
  }
  
  # นับเฉพาะ active players (ไม่ folded) สำหรับ position calculation
  active_players <- which(vapply(game_state$players,
                                 function(p) !isTRUE(p$folded), logical(1)))
  n_active       <- length(active_players)
  
  game_state$deck              <- sample(create_deck())
  game_state$community_cards   <- character()
  game_state$pot               <- 0
  game_state$current_bet       <- game_state$big_blind
  game_state$last_raise_amount <- game_state$big_blind
  game_state$current_round     <- "preflop"
  game_state$game_started      <- TRUE
  
  # ล้าง timer state และ BB option tracking
  game_state$turn_started_at <- NULL
  game_state$showdown_at     <- NULL
  game_state$bb_player_id    <- NULL   # จะถูก set ใน post_blinds()
  game_state$preflop_opened  <- FALSE  # มีใคร raise แล้วหรือยัง
  
  post_blinds()
  deal_hole_cards()
  
  # UTG position (คิดจาก active players เท่านั้น)
  # heads-up: dealer (SB) acts first preflop
  # 3+ players: UTG = player ถัดจาก BB
  dp_active_idx <- which(active_players == game_state$dealer_position)
  if (length(dp_active_idx) == 0L) dp_active_idx <- 1L
  
  first_active_idx <- if (n_active == 2L) {
    dp_active_idx
  } else {
    ((dp_active_idx + 1L) %% n_active) + 1L  # skip SB, skip BB → UTG
  }
  first <- active_players[first_active_idx]
  
  # หา player แรกที่ยังไม่ all-in
  for (offset in seq_len(n_active)) {
    idx <- ((first_active_idx - 1L + offset - 1L) %% n_active) + 1L
    pos <- active_players[idx]
    p   <- game_state$players[[pos]]
    if (!isTRUE(p$all_in)) {
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
  n  <- length(game_state$players)
  dp <- game_state$dealer_position
  sb <- (dp %% n) + 1L
  bb <- ((dp + 1L) %% n) + 1L
  
  .post <- function(pos, amount_full, label) {
    p      <- game_state$players[[pos]]
    amount <- min(amount_full, p$chips)
    game_state$players[[pos]]$bet_this_round      <- amount
    game_state$players[[pos]]$total_bet_this_hand <- amount
    game_state$players[[pos]]$chips               <- p$chips - amount
    game_state$pot                                <- game_state$pot + amount
    if ((p$chips - amount) <= 0) game_state$players[[pos]]$all_in <- TRUE
    log_action(paste(p$name, label, "$", amount))
  }
  
  .post(sb, game_state$small_blind, "posts SB")
  .post(bb, game_state$big_blind,   "posts BB")
  # Track BB position สำหรับ BB option preflop
  game_state$bb_player_id <- bb
}

# ---- Deal cards ----

deal_hole_cards <- function() {
  # Deal เฉพาะ player ที่ยังไม่ folded (skip mid-hand joiners)
  for (i in seq_along(game_state$players)) {
    if (!isTRUE(game_state$players[[i]]$folded)) {
      game_state$players[[i]]$cards <- game_state$deck[1:2]
      game_state$deck <- game_state$deck[-(1:2)]
    }
  }
}

deal_community <- function(n_cards) {
  game_state$deck        <- game_state$deck[-1]   # burn card
  cards                  <- game_state$deck[seq_len(n_cards)]
  game_state$community_cards <- c(game_state$community_cards, cards)
  game_state$deck        <- game_state$deck[-seq_len(n_cards)]
  return(cards)
}

# ---- Player actions ----

player_fold <- function(player_id) {
  p <- game_state$players[[player_id]]
  game_state$players[[player_id]]$folded <- TRUE
  game_state$turn_started_at <- NULL
  
  s <- p$stats$current_streak
  game_state$players[[player_id]]$stats$current_streak <-
    if (s <= 0L) s - 1L else -1L
  
  game_state$players[[player_id]]$acted <- TRUE
  log_action(paste(p$name, "folds"))
  if (!is.null(game_state$bb_player_id) &&
      player_id == game_state$bb_player_id) {
    game_state$preflop_opened <- TRUE
  }
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
  
  game_state$players[[player_id]]$acted <- TRUE
  action <- if (call_amount == 0) "checks" else paste("calls $", call_amount)
  log_action(paste(p$name, action))
  
  # ถ้า BB check (BB option ใช้แล้ว) → ปิด BB option
  if (game_state$current_round == "preflop" &&
      !is.null(game_state$bb_player_id) &&
      player_id == game_state$bb_player_id) {
    game_state$preflop_opened <- TRUE
  }
  
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
  game_state$pot               <- game_state$pot + total
  game_state$current_bet       <- p$bet_this_round + total
  game_state$last_raise_amount <- raise_amount
  game_state$turn_started_at   <- NULL
  
  game_state$players[[player_id]]$acted <- TRUE
  # ถ้า raise → reset acted ของคนอื่นทุกคน (ต้องตอบสนองต่อ raise)
  for (i in seq_along(game_state$players)) {
    if (i != player_id) game_state$players[[i]]$acted <- FALSE
  }
  if (game_state$current_round == "preflop") {
    game_state$preflop_opened <- TRUE
  }
  log_action(paste(p$name, "raises to $", game_state$current_bet))
  next_player()
  return(list(success = TRUE, error = NULL))
}

# ---- Turn advancement ----

next_player <- function() {
  n <- length(game_state$players)
  
  # เหลือ player เดียวที่ไม่ fold → จบทันที
  active_ids <- which(vapply(game_state$players,
                             function(p) !isTRUE(p$folded), logical(1)))
  if (length(active_ids) <= 1L) {
    end_betting_round()
    return()
  }
  
  # player ที่ยังต้อง act = ยังไม่ได้ act ในรอบนี้ หรือยังไม่ได้ match current_bet
  need_act <- sum(vapply(game_state$players, function(p)
    !isTRUE(p$folded) && !isTRUE(p$all_in) &&
      (!isTRUE(p$acted) || p$bet_this_round < game_state$current_bet),
    logical(1)))
  
  # BB Option: preflop เท่านั้น ถ้ายังไม่มีใคร raise
  bb_id <- game_state$bb_player_id
  bb_has_option <- (
    !is.null(bb_id) &&
      game_state$current_round == "preflop" &&
      !isTRUE(game_state$preflop_opened) &&
      need_act == 0L &&
      bb_id >= 1L && bb_id <= length(game_state$players) &&
      !isTRUE(game_state$players[[bb_id]]$folded) &&
      !isTRUE(game_state$players[[bb_id]]$all_in)
  )
  
  if (need_act == 0L && !bb_has_option) {
    end_betting_round()
    return()
  }
  
  if (need_act == 0L && bb_has_option) {
    start_turn_timer(bb_id)
    return()
  }
  
  # วน หา player ถัดไปที่ต้อง act
  start <- game_state$current_turn
  for (step in seq_len(n)) {
    pos <- (start %% n) + 1L
    start <- pos
    p <- game_state$players[[pos]]
    if (!isTRUE(p$folded) && !isTRUE(p$all_in) &&
        (!isTRUE(p$acted) || p$bet_this_round < game_state$current_bet)) {
      start_turn_timer(pos)
      return()
    }
  }
  end_betting_round()
}

end_betting_round <- function() {
  n <- length(game_state$players)
  
  active <- sum(vapply(game_state$players,
                       function(p) !isTRUE(p$folded), logical(1)))
  if (active <= 1L) {
    showdown()
    return()
  }
  
  # Reset round bets และ acted flag
  for (i in seq_len(n)) {
    game_state$players[[i]]$bet_this_round <- 0
    game_state$players[[i]]$acted          <- FALSE  # reset: ยังไม่ได้ act รอบนี้
  }
  game_state$current_bet       <- 0
  game_state$last_raise_amount <- 0
  game_state$turn_started_at   <- NULL
  # BB option หมดอายุหลัง preflop
  game_state$bb_player_id   <- NULL
  game_state$preflop_opened <- TRUE
  
  # Advance street
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
    # river → showdown
    showdown()
    return()
  }
  
  # FIX #4: ถ้าทุกคน all-in ให้ deal board ต่อเลย ไม่ต้องรอ action
  can_act <- sum(vapply(game_state$players, function(p)
    !isTRUE(p$folded) && !isTRUE(p$all_in), logical(1)))
  
  if (can_act == 0L) {
    end_betting_round()
    return()
  }
  
  # หา first to act (left of dealer)
  first <- (game_state$dealer_position %% n) + 1L
  for (offset in seq_len(n)) {
    pos <- ((first + offset - 2L) %% n) + 1L
    p   <- game_state$players[[pos]]
    if (!isTRUE(p$folded) && !isTRUE(p$all_in)) {
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
  game_state$current_turn    <- 0L
  
  # FIX #3: Deal board ให้ครบก่อน evaluate
  # ถ้า community cards ยังไม่ครบ 5 ใบ ให้ deal ต่อก่อน
  board_needed <- 5L - length(game_state$community_cards)
  if (board_needed > 0L) {
    if (board_needed == 5L) {
      # ยังไม่ได้ deal เลย (all-in preflop)
      cards <- deal_community(3); log_action(paste("FLOP:", format_cards(cards)))
      cards <- deal_community(1); log_action(paste("TURN:", format_card(cards)))
      cards <- deal_community(1); log_action(paste("RIVER:", format_card(cards)))
    } else if (board_needed == 2L) {
      cards <- deal_community(1); log_action(paste("TURN:", format_card(cards)))
      cards <- deal_community(1); log_action(paste("RIVER:", format_card(cards)))
    } else if (board_needed == 1L) {
      cards <- deal_community(1); log_action(paste("RIVER:", format_card(cards)))
    }
  }
  
  # FIX #2: log showdown header ก่อน แล้วค่อย log hands
  log_action("=== SHOWDOWN ===")
  
  for (p in game_state$players) {
    if (!isTRUE(p$folded) && length(p$cards) == 2L) {
      hand_result <- evaluate_hand(c(p$cards, game_state$community_cards))
      log_action(paste(p$name, ":", format_cards(p$cards), "->", hand_result$name))
    }
  }
  
  pots <- calculate_side_pots(game_state$players)
  game_state$players <- distribute_pots(pots, game_state$players,
                                        game_state$community_cards)
  
  game_state$dealer_position <-
    (game_state$dealer_position %% length(game_state$players)) + 1L
  
  game_state$showdown_at <- Sys.time()
}

# ---- Timer ----

start_turn_timer <- function(player_id) {
  game_state$current_turn    <- player_id
  game_state$turn_started_at <- Sys.time()
}

get_time_remaining <- function() {
  if (is.null(game_state$turn_started_at)) return(game_state$turn_timer_seconds)
  elapsed   <- as.numeric(difftime(Sys.time(), game_state$turn_started_at, units = "secs"))
  remaining <- game_state$turn_timer_seconds - elapsed
  max(0, round(remaining))
}

check_and_handle_timeout <- function() {
  if (!isTRUE(game_state$game_started))    return()
  if (is.null(game_state$turn_started_at)) return()
  if (game_state$current_round == "showdown") return()
  
  elapsed <- as.numeric(difftime(Sys.time(), game_state$turn_started_at, units = "secs"))
  if (elapsed < game_state$turn_timer_seconds) return()
  
  pid <- game_state$current_turn
  if (pid < 1L || pid > length(game_state$players)) return()
  p <- game_state$players[[pid]]
  if (!isTRUE(p$folded) && !isTRUE(p$all_in)) {
    log_action(paste(p$name, "timed out - auto fold"))
    player_fold(pid)
  }
}

# ---- Utility ----

log_action <- function(msg) {
  ts  <- format(Sys.time(), "%H:%M:%S")
  new <- paste0("[", ts, "] ", msg)
  existing <- game_state$action_log
  game_state$action_log <- c(new, existing)[seq_len(min(60L, length(existing) + 1L))]
}