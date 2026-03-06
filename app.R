# ============================================
# POKER LAN GAME - MAIN APP
# ============================================

library(shiny)
library(shinyjs)

# Source all logic files
source("R/poker_logic.R")
source("R/betting.R")
source("R/timer.R")
source("R/game_state.R")

# ============================================
# UI
# ============================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    tags$title("🃏 Poker LAN Game")
  ),
  
  div(class = "main-panel",
      
      # Header
      div(class = "game-header",
          div(class = "game-title", "♠♥ POKER LAN ♦♣"),
          div(class = "game-subtitle", "Texas Hold'em - No Limit")
      ),
      
      # Join Section (when not joined)
      conditionalPanel(
        condition = "output.player_id == null || output.player_id == ''",
        div(class = "join-section",
            h3("Join the Game"),
            textInput("player_name", NULL,
                      placeholder = "Enter your name",
                      width = "100%"),
            actionButton("join_game", "JOIN GAME", 
                         class = "btn btn-join")
        )
      ),
      
      # Main Game (when joined)
      conditionalPanel(
        condition = "output.player_id != null && output.player_id != ''",
        
        # Info Bar
        div(class = "info-bar",
            div(class = "info-item",
                div(class = "info-label", "Players"),
                div(class = "info-value",
                    textOutput("player_count", inline = TRUE))
            ),
            div(class = "info-item",
                div(class = "info-label", "Pot"),
                div(class = "info-value",
                    "$", textOutput("pot_display", inline = TRUE))
            ),
            div(class = "info-item",
                div(class = "info-label", "Round"),
                div(class = "info-value",
                    textOutput("round_display", inline = TRUE))
            )
        ),
        
        # Your Info
        div(class = "player-section",
            h4(textOutput("player_name_display", inline = TRUE)),
            div(class = "player-info",
                div(class = "stat-label", "Chips:"),
                div(class = "stat-value",
                    "$", textOutput("player_chips", inline = TRUE))
            )
        ),
        
        # Other Players
        conditionalPanel(
          condition = "output.player_id != null && output.player_id != ''",
          div(style = "margin: 20px 0;",
              h4(style = "color: #ffd700; margin-bottom: 15px;", 
                 "👥 Other Players"),
              div(class = "other-players",
                  uiOutput("other_players_display")
              )
          )
        ),
        # ✅ เพิ่มส่วนนี้ (START BUTTON) ใหม่ทั้งหมด
        conditionalPanel(
          condition = "output.player_id != null && output.player_id != ''",
          div(id = "start_game_section",
              uiOutput("start_game_button")
          )
        ),
        
        # Community Cards
        conditionalPanel(
          condition = "output.game_started == 'true'",
          div(class = "community-section",
              h4("Community Cards"),
              div(class = "community-cards",
                  uiOutput("community_cards_display")
              )
          )
        ),
        
        # Your Cards
        conditionalPanel(
          condition = "output.game_started == 'true'",
          div(class = "cards-section",
              h4("Your Hand"),
              div(class = "cards-container",
                  uiOutput("player_cards_display")
              )
          )
        ),
        
        # Actions (Your Turn)
        conditionalPanel(
          condition = "output.is_your_turn == 'true'",
          div(class = "actions-section",
              div(class = "turn-indicator",
                  "🎯 YOUR TURN"
              ),
              div(class = "timer-display",
                  uiOutput("timer_display")
              ),
              div(class = "action-buttons",
                  actionButton("action_fold", "FOLD",
                               class = "btn btn-fold"),
                  actionButton("action_call",
                               uiOutput("call_button_text", inline = TRUE),
                               class = "btn btn-call")
              ),
              div(class = "quick-bets",
                  h5("Quick Bets"),
                  div(class = "quick-bet-row",
                      actionButton("bet_third", 
                                   uiOutput("bet_third_text", inline = TRUE),
                                   class = "btn btn-quick"),
                      actionButton("bet_half",
                                   uiOutput("bet_half_text", inline = TRUE),
                                   class = "btn btn-quick"),
                      actionButton("bet_pot",
                                   uiOutput("bet_pot_text", inline = TRUE),
                                   class = "btn btn-quick"),
                      actionButton("bet_allin",
                                   uiOutput("bet_allin_text", inline = TRUE),
                                   class = "btn btn-allin")
                  )
              ),
              div(class = "custom-raise",
                  h5("Custom Raise"),
                  div(class = "raise-input-group",
                      numericInput("custom_raise_amount", NULL,
                                   value = 20,
                                   min = 10,
                                   step = 5,
                                   width = "100%"),
                      actionButton("action_raise", "RAISE",
                                   class = "btn btn-raise")
                  ),
                  textOutput("min_raise_text")
              )
          )
        ),
        
        # Waiting (Not Your Turn)
        conditionalPanel(
          condition = "output.is_your_turn == 'false' && output.game_started == 'true'",
          div(class = "waiting-section",
              div(class = "waiting-text", "⏳ Waiting for"),
              div(class = "waiting-player",
                  textOutput("current_player_name", inline = TRUE))
          )
        ),
        
        # Stats
        div(class = "stats-panel",
            h4("📊 Your Statistics"),
            div(class = "stat-row",
                span("Hands Played:"),
                span(textOutput("stat_hands_played", inline = TRUE))
            ),
            div(class = "stat-row",
                span("Hands Won:"),
                span(textOutput("stat_hands_won", inline = TRUE))
            ),
            div(class = "stat-row",
                span("Total Won:"),
                span("$", textOutput("stat_total_won", inline = TRUE))
            ),
            div(class = "stat-row",
                span("Biggest Pot:"),
                span("$", textOutput("stat_biggest_pot", inline = TRUE))
            ),
            div(class = "stat-row",
                span("Streak:"),
                uiOutput("stat_streak")
            )
        ),
        
        # Game Log
        div(class = "log-section",
            h4("📜 Game Log"),
            div(class = "game-log",
                uiOutput("game_log_display")
            )
        )
      )
  )
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {
  
  # Initialize game state as reactiveValues
  game_state <<- reactiveValues(
    # Players
    players = list(),
    max_players = 9,
    
    # Game status
    game_started = FALSE,
    current_round = "waiting",
    
    # Cards
    deck = character(),
    community_cards = character(),
    
    # Betting
    pot = 0,
    side_pots = list(),
    current_bet = 0,
    last_raise_amount = 0,
    small_blind = 5,
    big_blind = 10,
    
    # Positions
    dealer_position = 1,
    current_turn = 0,
    
    # Timer
    turn_started_at = NULL,
    turn_timer_seconds = 30,
    
    # Log
    action_log = character()
  )
  
  # Player's ID (session-specific)
  player_id <- reactiveVal(NULL)
  
  # Auto-refresh timer
  autoInvalidate <- reactiveTimer(1000)
  
  # Timeout checker
  observe({
    autoInvalidate()
    
    if (game_state$game_started && !is.null(game_state$turn_started_at)) {
      handle_timeout()
    }
  })
  
  # ==========================================
  # Event Handlers
  # ==========================================
  
  # Join Game
  observeEvent(input$join_game, {
    req(input$player_name)
    
    # Check if already joined
    if (!is.null(player_id())) {
      showNotification("You already joined!", type = "warning")
      return()
    }
    
    name <- trimws(input$player_name)
    
    if (nchar(name) == 0) {
      showNotification("Please enter your name", type = "error")
      return()
    }
    
    pid <- add_player(name)
    
    if (is.null(pid)) {
      showNotification("Game is full!", type = "error")
    } else {
      player_id(pid)
      
      # Debug log
      cat("\n=== PLAYER JOINED ===\n")
      cat("Session ID:", session$token, "\n")
      cat("Player ID:", pid, "\n")
      cat("Player Name:", game_state$players[[pid]]$name, "\n")
      cat("Total Players:", length(game_state$players), "\n")
      cat("All Players:\n")
      for (p in game_state$players) {
        cat("  - ID:", p$id, "Name:", p$name, "\n")
      }
      cat("====================\n\n")
      
      # Get actual name (might be modified if duplicate)
      actual_name <- game_state$players[[pid]]$name
      
      if (actual_name != name) {
        showNotification(paste("Name taken. You are:", actual_name), 
                         type = "warning")
      } else {
        showNotification(paste("Welcome,", actual_name, "!"), type = "message")
      }
    }
  })
  
  # Start Game
  observeEvent(input$start_game, {
    req(player_id())
    
    success <- start_game()
    
    if (!success) {
      showNotification("Need at least 2 players to start!", type = "error")
    } else {
      showNotification("New hand started!", type = "message")
    }
  })
  
  # Fold
  observeEvent(input$action_fold, {
    req(player_id())
    
    if (game_state$current_turn == player_id()) {
      player_fold(player_id())
    }
  })
  
  # Call/Check
  observeEvent(input$action_call, {
    req(player_id())
    
    if (game_state$current_turn == player_id()) {
      player_call(player_id())
    }
  })
  
  # Raise
  observeEvent(input$action_raise, {
    req(player_id(), input$custom_raise_amount)
    
    if (game_state$current_turn == player_id()) {
      result <- player_raise(player_id(), input$custom_raise_amount)
      
      if (!result$success) {
        showNotification(result$error, type = "error")
      }
    }
  })
  
  # Quick Bet: 1/3 Pot
  observeEvent(input$bet_third, {
    req(player_id())
    
    if (game_state$current_turn == player_id()) {
      amount <- max(get_min_raise(), ceiling(game_state$pot / 3))
      result <- player_raise(player_id(), amount)
      
      if (!result$success) {
        showNotification(result$error, type = "error")
      }
    }
  })
  
  # Quick Bet: 1/2 Pot
  observeEvent(input$bet_half, {
    req(player_id())
    
    if (game_state$current_turn == player_id()) {
      amount <- max(get_min_raise(), ceiling(game_state$pot / 2))
      result <- player_raise(player_id(), amount)
      
      if (!result$success) {
        showNotification(result$error, type = "error")
      }
    }
  })
  
  # Quick Bet: Pot
  observeEvent(input$bet_pot, {
    req(player_id())
    
    if (game_state$current_turn == player_id()) {
      amount <- max(get_min_raise(), game_state$pot)
      result <- player_raise(player_id(), amount)
      
      if (!result$success) {
        showNotification(result$error, type = "error")
      }
    }
  })
  
  # Quick Bet: All-In
  observeEvent(input$bet_allin, {
    req(player_id())
    
    if (game_state$current_turn == player_id()) {
      player <- game_state$players[[player_id()]]
      call_amount <- game_state$current_bet - player$bet_this_round
      
      if (call_amount >= player$chips) {
        # Just call all-in (can't raise)
        player_call(player_id())
      } else {
        # Raise all remaining chips
        raise_amount <- player$chips - call_amount
        result <- player_raise(player_id(), raise_amount)
        
        if (!result$success) {
          # If raise fails, just call all-in
          player_call(player_id())
        }
      }
    }
  })
  
  # ==========================================
  # Outputs - Basic Info
  # ==========================================
  
  output$player_id <- reactive({
    as.character(player_id())
  })
  outputOptions(output, "player_id", suspendWhenHidden = FALSE)
  
  output$game_started <- reactive({
    as.character(game_state$game_started)
  })
  outputOptions(output, "game_started", suspendWhenHidden = FALSE)
  
  output$start_game_button <- renderUI({
    req(player_id())
    autoInvalidate()
    
    # Only show if game not started
    if (game_state$game_started) {
      return(NULL)
    }
    
    # Show START button
    div(style = "margin: 30px 0; padding: 30px; background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%); border-radius: 15px; text-align: center; box-shadow: 0 10px 30px rgba(255, 215, 0, 0.5);",
        h2(style = "color: #000; margin: 0 0 15px 0; font-size: 32px;", 
           "🎮 READY TO PLAY? 🎮"),
        p(style = "color: #333; margin: 0 0 10px 0; font-size: 18px;",
          paste0("Players joined: ", length(game_state$players), "/", game_state$max_players)),
        p(style = "color: #666; margin: 0 0 20px 0; font-size: 14px;",
          "Need at least 2 players to start"),
        actionButton("start_game", 
                     "🎲 START NEW HAND 🎲",
                     class = "btn",
                     style = "font-size: 28px; padding: 25px 50px; background: #000; color: #ffd700; border: 3px solid #fff; width: 100%; font-weight: bold; cursor: pointer;")
    )
  })
  
  output$is_your_turn <- reactive({
    req(player_id())
    autoInvalidate()
    
    # Safety check
    if (player_id() > length(game_state$players)) {
      return("false")
    }
    
    as.character(
      game_state$current_turn == player_id() &&
        game_state$game_started &&
        game_state$current_round != "showdown" &&
        !game_state$players[[player_id()]]$folded &&
        !game_state$players[[player_id()]]$all_in
    )
  })
  outputOptions(output, "is_your_turn", suspendWhenHidden = FALSE)
  
  output$player_count <- renderText({
    autoInvalidate()
    paste0(length(game_state$players), "/", game_state$max_players)
  })
  
  output$pot_display <- renderText({
    autoInvalidate()
    format(game_state$pot, big.mark = ",")
  })
  
  output$round_display <- renderText({
    autoInvalidate()
    toupper(game_state$current_round)
  })
  
  # ==========================================
  # Outputs - Player Info
  # ==========================================
  
  output$player_name_display <- renderText({
    req(player_id())
    
    if (player_id() > length(game_state$players)) {
      return("")
    }
    
    paste("👤", game_state$players[[player_id()]]$name)
  })
  
  output$player_chips <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("0")
    }
    
    format(game_state$players[[player_id()]]$chips, big.mark = ",")
  })
  
  # ==========================================
  # Outputs - Cards
  # ==========================================
  
  output$player_cards_display <- renderUI({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return(p("No cards yet", style = "color: #999;"))
    }
    
    cards <- game_state$players[[player_id()]]$cards
    
    if (length(cards) == 0) {
      return(p("No cards yet", style = "color: #999;"))
    }
    
    lapply(cards, function(card) {
      color <- if (grepl("[♥♦]", card)) "card-red" else "card-black"
      div(class = paste("card", color), card)
    })
  })
  
  output$community_cards_display <- renderUI({
    autoInvalidate()
    
    cards <- game_state$community_cards
    total_needed <- 5
    
    # Show actual cards
    card_divs <- if (length(cards) > 0) {
      lapply(cards, function(card) {
        color <- if (grepl("[♥♦]", card)) "card-red" else "card-black"
        div(class = paste("card", color), card)
      })
    } else {
      list()
    }
    
    # Add back cards for unrevealed
    n_back <- total_needed - length(cards)
    if (n_back > 0) {
      back_cards <- lapply(1:n_back, function(i) {
        div(class = "card card-back", "?")
      })
      card_divs <- c(card_divs, back_cards)
    }
    
    tagList(card_divs)
  })
  
  # ==========================================
  # Outputs - Other Players
  # ==========================================
  
  output$other_players_display <- renderUI({
    req(player_id())
    autoInvalidate()
    
    # Safety check
    if (is.null(player_id()) || player_id() < 1 || 
        player_id() > length(game_state$players)) {
      return(p("Error: Invalid player ID", style = "color: red;"))
    }
    
    # Get all players
    all_players <- game_state$players
    
    if (length(all_players) <= 1) {
      return(p("No other players yet", style = "color: #999;"))
    }
    
    # Filter out current player
    current_pid <- player_id()
    other_players <- list()
    
    for (p in all_players) {
      if (p$id != current_pid) {
        other_players[[length(other_players) + 1]] <- p
      }
    }
    
    if (length(other_players) == 0) {
      return(p("No other players yet", style = "color: #999;"))
    }
    
    # Display other players
    lapply(other_players, function(p) {
      is_active <- game_state$current_turn == p$id && 
        game_state$game_started &&
        !p$folded && !p$all_in
      
      is_folded <- p$folded
      
      card_class <- "player-card"
      if (is_active) card_class <- paste(card_class, "active")
      if (is_folded) card_class <- paste(card_class, "folded")
      
      status_text <- if (is_folded) {
        "FOLDED"
      } else if (p$all_in) {
        "ALL-IN"
      } else if (is_active) {
        "THINKING..."
      } else {
        "ACTIVE"
      }
      
      status_class <- if (is_folded) {
        "status-folded"
      } else if (is_active) {
        "status-thinking"
      } else if (p$all_in) {
        "status-allin"
      } else {
        ""
      }
      
      div(class = card_class,
          div(class = "player-name", p$name),
          div(class = "player-chips",
              paste0("💰 $", format(p$chips, big.mark = ","))),
          if (p$bet_this_round > 0) {
            div(class = "player-bet",
                paste0("Bet: $", p$bet_this_round))
          },
          div(class = paste("player-status", status_class),
              status_text)
      )
    })
  })
  
  # ==========================================
  # Outputs - Actions
  # ==========================================
  
  output$timer_display <- renderUI({
    autoInvalidate()
    
    remaining <- get_time_remaining()
    
    timer_class <- if (remaining > 15) {
      "timer-ok"
    } else if (remaining > 5) {
      "timer-warning"
    } else {
      "timer-danger"
    }
    
    div(class = timer_class,
        paste0("⏱️ ", remaining, "s"))
  })
  
  output$call_button_text <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("CALL")
    }
    
    player <- game_state$players[[player_id()]]
    call_amount <- game_state$current_bet - player$bet_this_round
    
    if (call_amount == 0) {
      "CHECK"
    } else {
      paste0("CALL $", call_amount)
    }
  })
  
  output$bet_third_text <- renderText({
    autoInvalidate()
    amount <- max(get_min_raise(), ceiling(game_state$pot / 3))
    paste0("1/3 POT ($", amount, ")")
  })
  
  output$bet_half_text <- renderText({
    autoInvalidate()
    amount <- max(get_min_raise(), ceiling(game_state$pot / 2))
    paste0("1/2 POT ($", amount, ")")
  })
  
  output$bet_pot_text <- renderText({
    autoInvalidate()
    amount <- max(get_min_raise(), game_state$pot)
    paste0("POT ($", amount, ")")
  })
  
  output$bet_allin_text <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("ALL-IN ($0)")
    }
    
    player <- game_state$players[[player_id()]]
    paste0("ALL-IN ($", player$chips, ")")
  })
  
  output$min_raise_text <- renderText({
    autoInvalidate()
    paste("Minimum raise: $", get_min_raise())
  })
  
  output$current_player_name <- renderText({
    autoInvalidate()
    
    if (game_state$current_turn > 0 && 
        game_state$current_turn <= length(game_state$players)) {
      game_state$players[[game_state$current_turn]]$name
    } else {
      "..."
    }
  })
  
  # ==========================================
  # Outputs - Stats
  # ==========================================
  
  output$stat_hands_played <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("0")
    }
    
    game_state$players[[player_id()]]$stats$hands_played
  })
  
  output$stat_hands_won <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("0 (0%)")
    }
    
    stats <- game_state$players[[player_id()]]$stats
    won <- stats$hands_won
    played <- max(1, stats$hands_played)
    pct <- round(won / played * 100, 1)
    
    paste0(won, " (", pct, "%)")
  })
  
  output$stat_total_won <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("0")
    }
    
    format(game_state$players[[player_id()]]$stats$total_won, big.mark = ",")
  })
  
  output$stat_biggest_pot <- renderText({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return("0")
    }
    
    format(game_state$players[[player_id()]]$stats$biggest_pot, big.mark = ",")
  })
  
  output$stat_streak <- renderUI({
    req(player_id())
    autoInvalidate()
    
    if (player_id() > length(game_state$players)) {
      return(span("—"))
    }
    
    streak <- game_state$players[[player_id()]]$stats$current_streak
    
    if (streak > 0) {
      span(class = "streak-positive",
           paste0("🔥 ", streak, " Win", if (streak > 1) "s" else ""))
    } else if (streak < 0) {
      span(class = "streak-negative",
           paste0("❄️ ", abs(streak), " Loss", if (abs(streak) > 1) "es" else ""))
    } else {
      span("—")
    }
  })
  
  # ==========================================
  # Outputs - Game Log
  # ==========================================
  
  output$game_log_display <- renderUI({
    autoInvalidate()
    
    logs <- game_state$action_log
    
    if (length(logs) == 0) {
      return(HTML("<p style='color: #666;'>No actions yet...</p>"))
    }
    
    HTML(paste(logs, collapse = "<br>"))
  })
  
  # ==========================================
  # Session End Handler
  # ==========================================
  
  session$onSessionEnded(function() {
    # Player disconnected
    pid <- isolate(player_id())
    
    if (!is.null(pid) && pid > 0 && pid <= length(game_state$players)) {
      player_name <- game_state$players[[pid]]$name
      
      # Auto-fold if it was their turn
      if (game_state$game_started && 
          game_state$current_turn == pid &&
          !game_state$players[[pid]]$folded) {
        log_action(paste(player_name, "disconnected - auto fold"))
        player_fold(pid)
      }
    }
  })
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)