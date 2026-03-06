# ============================================================
# global.R  –  sourced by Shiny in the TRUE global environment
# before app.R, ensuring game_state and all helper functions
# share the same lexical scope.
# ============================================================

library(shiny)
library(shinyjs)

source("R/poker_logic.R")
source("R/betting.R")
source("R/game_state.R")
source("R/stats.R")

# ---- Shared game state (one instance per server process, all sessions) ----
game_state <- reactiveValues(
  players          = list(),
  max_players      = 9L,
  starting_chips   = 1000,
  game_started     = FALSE,
  current_round    = "waiting",
  deck             = character(),
  community_cards  = character(),
  pot              = 0,
  current_bet      = 0,
  last_raise_amount = 0,
  small_blind      = 5,
  big_blind        = 10,
  dealer_position  = 1L,
  current_turn     = 0L,
  turn_started_at  = NULL,
  turn_timer_seconds = 30L,
  showdown_at      = NULL,
  action_log       = character()
)
