# ============================================
# TURN TIMER
# ============================================

#' Start turn timer for player
#' @param player_id ID of player
start_turn_timer <- function(player_id) {
  if (!exists("game_state")) return()
  
  game_state$current_turn <- player_id
  game_state$turn_started_at <- Sys.time()
}

#' Get remaining time in seconds
#' @return numeric seconds remaining
get_time_remaining <- function() {
  if (!exists("game_state")) return(30)
  
  if (is.null(game_state$turn_started_at)) {
    return(game_state$turn_timer_seconds)
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), 
                                 game_state$turn_started_at,
                                 units = "secs"))
  
  remaining <- game_state$turn_timer_seconds - elapsed
  
  return(max(0, round(remaining)))
}

#' Check if current turn has timed out
#' @return logical TRUE if timed out
check_timeout <- function() {
  if (!exists("game_state")) return(FALSE)
  
  if (is.null(game_state$turn_started_at)) {
    return(FALSE)
  }
  
  elapsed <- as.numeric(difftime(Sys.time(),
                                 game_state$turn_started_at,
                                 units = "secs"))
  
  return(elapsed >= game_state$turn_timer_seconds)
}

#' Handle timeout - auto fold player
handle_timeout <- function() {
  if (!exists("game_state")) return()
  
  if (!check_timeout()) {
    return()
  }
  
  if (!game_state$game_started) {
    return()
  }
  
  player_id <- game_state$current_turn
  
  if (player_id < 1 || player_id > length(game_state$players)) {
    return()
  }
  
  player <- game_state$players[[player_id]]
  
  if (!player$folded && !player$all_in) {
    log_action(paste(player$name, "⏰ TIMED OUT - auto fold"))
    player_fold(player_id)
  }
}