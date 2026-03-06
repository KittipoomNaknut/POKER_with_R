# ============================================================
# POKER LAN GAME  –  app.R
# Texas Hold'em, multi-player, with win-probability stats
# ============================================================

library(shiny)
library(shinyjs)

source("R/poker_logic.R")
source("R/betting.R")
source("R/game_state.R")
source("R/stats.R")

# ============================================================
# SHARED GAME STATE
#
# game_state ต้องอยู่ใน .GlobalEnv เพราะ functions ใน R/*.R
# ใช้ lexical scoping — พวกมัน lookup ชื่อ "game_state" ใน
# environment ที่ถูก define (global) ไม่ใช่ environment ของ caller
#
# reactiveValues() สร้างได้ใน global scope ของ Shiny app
# (ไม่ใช่ภายใน server function) — Shiny รองรับ pattern นี้
# สำหรับ shared state ข้าม sessions
# ============================================================
if (!exists("game_state", envir = .GlobalEnv)) {
  game_state <<- reactiveValues(
    players            = list(),
    max_players        = 9L,
    starting_chips     = 1000,
    game_started       = FALSE,
    current_round      = "waiting",
    deck               = character(),
    community_cards    = character(),
    pot                = 0,
    current_bet        = 0,
    last_raise_amount  = 0,
    small_blind        = 5,
    big_blind          = 10,
    dealer_position    = 1L,
    current_turn       = 0L,
    turn_started_at    = NULL,
    turn_timer_seconds = 30L,
    showdown_at        = NULL,
    action_log         = character(),
    bb_player_id       = NULL,
    preflop_opened     = FALSE
  )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "css/style.css"),
    tags$title("Poker LAN")
  ),
  
  # ---- Join panel (before joining) ----
  conditionalPanel(
    condition = "output.my_pid == ''",
    div(class = "section",
        h2("Poker LAN"),
        p("Texas Hold'em – No Limit"),
        hr(),
        textInput("inp_name", "Your name:", width = "280px"),
        actionButton("btn_join", "Join Game", class = "btn btn-primary"),
        hr(),
        h4("Players waiting:"),
        tableOutput("tbl_lobby")
    )
  ),
  
  # ---- Game panel (after joining) ----
  conditionalPanel(
    condition = "output.my_pid != ''",
    
    div(class = "info-bar",
        span(strong("Round: "), textOutput("txt_round", inline = TRUE)),
        span(strong("Pot: $"), textOutput("txt_pot",   inline = TRUE)),
        span(strong("Turn: "), textOutput("txt_turn",  inline = TRUE)),
        span(strong("Timer: "), textOutput("txt_timer", inline = TRUE), "s")
    ),
    
    fluidRow(
      # ---- Left: hand + actions ----
      column(4,
             div(class = "section",
                 h4(textOutput("txt_myname", inline = TRUE)),
                 p(strong("Chips: $"), textOutput("txt_mychips", inline = TRUE)),
                 h5("Your cards:"),
                 uiOutput("ui_mycards"),
                 uiOutput("ui_hand_strength")
             ),
             
             # แสดงปุ่ม action ตลอดเวลาที่ game กำลังเล่น
             conditionalPanel(
               condition = "output.game_on == 'true'",
               div(class = "section",
                   uiOutput("ui_turn_banner"),
                   div(class = "action-row",
                       actionButton("btn_fold",  "Fold",   class = "btn btn-danger"),
                       actionButton("btn_call",  "...",    class = "btn btn-success",
                                    style = "min-width:100px"),
                       actionButton("btn_allin", "All-In", class = "btn btn-warning")
                   ),
                   hr(),
                   h5("Raise:"),
                   div(class = "action-row",
                       actionButton("btn_pot3", "1/3 Pot", class = "btn btn-default"),
                       actionButton("btn_pot2", "1/2 Pot", class = "btn btn-default"),
                       actionButton("btn_pot1", "1x  Pot", class = "btn btn-default")
                   ),
                   div(class = "action-row",
                       numericInput("inp_raise", NULL, value = 20, min = 1, step = 5,
                                    width = "120px"),
                       actionButton("btn_raise", "Raise", class = "btn btn-primary")
                   ),
                   textOutput("txt_min_raise")
               )
             ),
             
             conditionalPanel(
               condition = "output.game_on != 'true'",
               div(class = "section",
                   uiOutput("ui_start_btn")
               )
             )
      ),
      
      # ---- Middle: community + win prob ----
      column(4,
             div(class = "section",
                 h4("Community cards"),
                 uiOutput("ui_community"),
                 hr(),
                 h4("Win probability"),
                 p(em("(Monte Carlo, vs. active opponents)")),
                 uiOutput("ui_winprob"),
                 tableOutput("tbl_hand_dist")
             )
      ),
      
      # ---- Right: others + log ----
      column(4,
             div(class = "section",
                 h4("Other players"),
                 tableOutput("tbl_others")
             ),
             div(class = "section",
                 h4("Game log"),
                 div(style = "max-height:300px;overflow-y:auto;font-size:12px;",
                     uiOutput("ui_log"))
             )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Per-session state
  my_pid         <- reactiveVal("")
  tick           <- reactiveTimer(1000)
  wp_cache       <- reactiveVal(NULL)
  prev_cards_key <- reactiveVal("")
  
  # ---- Background tick ----
  observe({
    tick()
    isolate({
      if (isTRUE(game_state$game_started)) check_and_handle_timeout()
      
      if (!is.null(game_state$showdown_at)) {
        elapsed <- as.numeric(difftime(Sys.time(), game_state$showdown_at, units = "secs"))
        if (elapsed >= 3) {
          game_state$game_started  <- FALSE
          game_state$current_round <- "waiting"
          game_state$pot           <- 0
          game_state$showdown_at   <- NULL
        }
      }
    })
  })
  
  # ---- Win probability cache ----
  observe({
    tick()
    pid <- isolate(my_pid())
    if (pid == "" || !isTRUE(game_state$game_started)) return()
    pid <- as.integer(pid)
    if (pid > length(game_state$players)) return()
    
    hole      <- game_state$players[[pid]]$cards
    community <- game_state$community_cards
    key       <- paste(c(hole, community), collapse = ",")
    
    isolate({
      if (key != prev_cards_key() && length(hole) == 2) {
        n_opp <- sum(vapply(game_state$players,
                            function(p) p$id != pid && !p$folded, logical(1)))
        n_opp <- max(1L, n_opp)
        res <- tryCatch(
          calc_win_prob(hole, community, n_opp, n_sim = 350L),
          error = function(e) NULL
        )
        wp_cache(res)
        prev_cards_key(key)
      }
    })
  })
  
  # ====================================================
  # Event handlers
  # ====================================================
  
  observeEvent(input$btn_join, {
    nm <- trimws(input$inp_name)
    if (nchar(nm) == 0) {
      showNotification("Enter a name first.", type = "error"); return()
    }
    if (my_pid() != "") {
      showNotification("Already joined.", type = "warning"); return()
    }
    pid <- add_player(nm)
    if (is.null(pid)) {
      showNotification("Game full or invalid name.", type = "error")
    } else {
      my_pid(as.character(pid))
      actual_name <- game_state$players[[pid]]$name
      note <- if (actual_name != nm)
        paste("Name taken – you are:", actual_name) else paste("Welcome,", actual_name)
      showNotification(note, type = "message")
    }
  })
  
  observeEvent(input$btn_start, {
    if (my_pid() == "") return()
    ok <- start_game()
    if (!ok) showNotification("Need at least 2 players.", type = "error")
  })
  
  # ตรวจสอบว่า player สามารถ act ได้ไหม
  # คืน TRUE ถ้า act ได้, แสดง notification และคืน FALSE ถ้าไม่ได้
  can_act <- function(pid) {
    if (!isTRUE(game_state$game_started)) {
      showNotification("Game not started yet.", type = "warning"); return(FALSE)
    }
    if (pid > length(game_state$players)) return(FALSE)
    p <- game_state$players[[pid]]
    if (isTRUE(p$folded)) {
      showNotification("You have already folded.", type = "warning"); return(FALSE)
    }
    if (isTRUE(p$all_in)) {
      showNotification("You are all-in.", type = "warning"); return(FALSE)
    }
    if (game_state$current_round == "showdown") {
      showNotification("Hand is over.", type = "warning"); return(FALSE)
    }
    if (!isTRUE(game_state$current_turn == pid)) {
      ct <- game_state$current_turn
      cur_name <- if (ct > 0L && ct <= length(game_state$players))
        game_state$players[[ct]]$name else "someone"
      showNotification(paste0("Not your turn! Waiting for ", cur_name, "."),
                       type = "warning", duration = 2)
      return(FALSE)
    }
    return(TRUE)
  }
  
  observeEvent(input$btn_fold, {
    pid <- as.integer(my_pid())
    if (can_act(pid)) player_fold(pid)
  })
  
  observeEvent(input$btn_call, {
    pid <- as.integer(my_pid())
    if (can_act(pid)) player_call(pid)
  })
  
  observeEvent(input$btn_allin, {
    pid <- as.integer(my_pid())
    if (!can_act(pid)) return()
    p          <- game_state$players[[pid]]
    call_amt   <- game_state$current_bet - p$bet_this_round
    raise_part <- p$chips - call_amt
    if (raise_part <= 0) {
      player_call(pid)
    } else {
      res <- player_raise(pid, raise_part)
      if (!isTRUE(res$success)) player_call(pid)
    }
  })
  
  observeEvent(input$btn_raise, {
    pid <- as.integer(my_pid())
    if (!can_act(pid)) return()
    raise_val <- input$inp_raise
    if (is.null(raise_val) || is.na(raise_val) || raise_val <= 0) {
      showNotification("Invalid raise amount.", type = "error"); return()
    }
    res <- player_raise(pid, raise_val)
    if (!isTRUE(res$success)) showNotification(res$error, type = "error")
  })
  
  .quick_bet <- function(fraction) {
    pid <- as.integer(my_pid())
    if (!can_act(pid)) return()
    base_amt <- ceiling(game_state$pot * fraction)
    amt      <- max(get_min_raise(), base_amt)
    res      <- player_raise(pid, amt)
    if (!isTRUE(res$success)) showNotification(res$error, type = "error")
  }
  observeEvent(input$btn_pot3, { .quick_bet(1/3) })
  observeEvent(input$btn_pot2, { .quick_bet(1/2) })
  observeEvent(input$btn_pot1, { .quick_bet(1)   })
  
  session$onSessionEnded(function() {
    isolate({
      pid_str <- my_pid()
      if (pid_str == "") return()
      pid <- as.integer(pid_str)
      if (pid < 1L || pid > length(game_state$players)) return()
      if (isTRUE(game_state$game_started) &&
          isTRUE(game_state$current_turn == pid) &&
          !isTRUE(game_state$players[[pid]]$folded)) {
        log_action(paste(game_state$players[[pid]]$name, "disconnected – auto fold"))
        player_fold(pid)
      }
    })
  })
  
  # ====================================================
  # Outputs
  # ====================================================
  
  output$my_pid <- reactive({ my_pid() })
  outputOptions(output, "my_pid", suspendWhenHidden = FALSE)
  
  output$game_on <- reactive({
    tick()
    if (isTRUE(game_state$game_started)) "true" else "false"
  })
  outputOptions(output, "game_on", suspendWhenHidden = FALSE)
  
  output$is_my_turn <- reactive({
    tick()
    pid_str <- my_pid()
    if (pid_str == "") return("false")
    pid <- as.integer(pid_str)
    if (pid > length(game_state$players)) return("false")
    p <- game_state$players[[pid]]
    is_turn <- (
      isTRUE(game_state$game_started)             &&
        isTRUE(game_state$current_turn == pid)       &&
        game_state$current_round != "showdown"       &&
        !isTRUE(p$folded) && !isTRUE(p$all_in)      &&
        !isTRUE(p$sit_out)
    )
    if (is_turn) "true" else "false"
  })
  outputOptions(output, "is_my_turn", suspendWhenHidden = FALSE)
  
  output$txt_round <- renderText({ tick(); toupper(game_state$current_round) })
  
  output$txt_pot <- renderText({
    tick(); format(game_state$pot, big.mark = ",")
  })
  
  output$txt_turn <- renderText({
    tick()
    ct <- game_state$current_turn
    if (ct > 0L && ct <= length(game_state$players))
      game_state$players[[ct]]$name else "—"
  })
  
  output$txt_timer <- renderText({ tick(); get_time_remaining() })
  
  output$txt_mychips <- renderText({
    pid_str <- my_pid(); if (pid_str == "") return("0")
    pid <- as.integer(pid_str); tick()
    if (pid > length(game_state$players)) return("0")
    format(game_state$players[[pid]]$chips, big.mark = ",")
  })
  
  output$ui_mycards <- renderUI({
    pid_str <- my_pid(); if (pid_str == "") return(NULL)
    pid <- as.integer(pid_str); tick()
    if (pid > length(game_state$players)) return(NULL)
    cards <- game_state$players[[pid]]$cards
    if (length(cards) == 0) return(p("(no cards)"))
    tagList(lapply(cards, function(cd) {
      s   <- card_suit(cd)
      cls <- if (s %in% c("h", "d")) "card red-card" else "card black-card"
      span(class = cls, format_card(cd))
    }))
  })
  
  output$ui_hand_strength <- renderUI({
    pid_str <- my_pid(); if (pid_str == "") return(NULL)
    pid <- as.integer(pid_str); tick()
    if (pid > length(game_state$players)) return(NULL)
    cards_h <- c(game_state$players[[pid]]$cards, game_state$community_cards)
    if (length(cards_h) < 5) return(NULL)
    p(strong("Hand: "), evaluate_hand(cards_h)$name)
  })
  
  observe({
    tick()
    pid_str <- my_pid()
    if (pid_str == "" || !isTRUE(game_state$game_started)) return()
    pid <- as.integer(pid_str)
    if (pid > length(game_state$players)) return()
    call_amt <- game_state$current_bet - game_state$players[[pid]]$bet_this_round
    label    <- if (call_amt == 0) "Check" else paste0("Call $", call_amt)
    updateActionButton(session, "btn_call", label = label)
  })
  
  output$txt_min_raise <- renderText({ tick(); paste("Min raise: $", get_min_raise()) })
  
  output$ui_community <- renderUI({
    tick()
    cards <- game_state$community_cards
    if (length(cards) == 0) return(p("(none yet)"))
    tagList(lapply(cards, function(cd) {
      s   <- card_suit(cd)
      cls <- if (s %in% c("h", "d")) "card red-card" else "card black-card"
      span(class = cls, format_card(cd))
    }))
  })
  
  output$ui_winprob <- renderUI({
    tick()
    pid_str <- my_pid()
    if (pid_str == "" || !isTRUE(game_state$game_started)) return(p("—"))
    pid <- as.integer(pid_str)
    if (pid > length(game_state$players))            return(p("—"))
    if (length(game_state$players[[pid]]$cards) < 2) return(p("—"))
    res <- wp_cache()
    if (is.null(res)) return(p("Calculating..."))
    tagList(p(strong("Win: "), paste0(res$win_pct, "%"),
              " | ",
              strong("Tie: "), paste0(res$tie_pct, "%")))
  })
  
  output$tbl_hand_dist <- renderTable({
    tick()
    res <- wp_cache()
    if (is.null(res) || is.null(res$hand_dist)) return(NULL)
    res$hand_dist
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "xs",
  caption = "Simulated hand distribution")
  
  output$tbl_lobby <- renderTable({
    tick()
    if (length(game_state$players) == 0)
      return(data.frame(Name = "(none)", Chips = "—", stringsAsFactors = FALSE))
    data.frame(
      Name  = vapply(game_state$players, `[[`, character(1), "name"),
      Chips = vapply(game_state$players,
                     function(p) paste0("$", p$chips), character(1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE)
  
  output$tbl_others <- renderTable({
    tick()
    pid_str <- my_pid(); if (pid_str == "") return(NULL)
    pid    <- as.integer(pid_str)
    others <- Filter(function(p) p$id != pid, game_state$players)
    if (length(others) == 0) return(data.frame(Info = "(none)"))
    data.frame(
      Name   = vapply(others, `[[`, character(1), "name"),
      Chips  = vapply(others,
                      function(p) paste0("$", format(p$chips, big.mark = ",")),
                      character(1)),
      Bet    = vapply(others,
                      function(p) paste0("$", p$bet_this_round), character(1)),
      Status = vapply(others, function(p) {
        if      (isTRUE(p$folded))  "FOLD"
        else if (isTRUE(p$all_in))  "ALL-IN"
        else if (isTRUE(game_state$current_turn == p$id) &&
                 isTRUE(game_state$game_started))          "THINKING"
        else                                               "active"
      }, character(1)),
      Won    = vapply(others, function(p) p$stats$hands_won, integer(1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE)
  
  output$ui_start_btn <- renderUI({
    tick()
    n <- length(game_state$players)
    tagList(
      p(paste0("Players: ", n, " / ", game_state$max_players)),
      actionButton("btn_start", "Start New Hand",
                   class    = "btn btn-lg btn-primary",
                   disabled = if (n < 2) "disabled" else NULL)
    )
  })
  
  output$txt_cur_player <- renderText({
    tick()
    ct <- game_state$current_turn
    if (ct > 0L && ct <= length(game_state$players))
      game_state$players[[ct]]$name else "..."
  })
  
  output$ui_waiting_status <- renderUI({
    tick()
    pid_str <- my_pid()
    if (pid_str == "") return(NULL)
    pid <- as.integer(pid_str)
    if (pid > length(game_state$players)) return(NULL)
    p <- game_state$players[[pid]]
    
    if (isTRUE(p$sit_out) || (isTRUE(p$folded) && length(p$cards) == 0)) {
      # Player join กลาง hand — แสดง sit-out message
      tagList(
        p(style = "color:#e67e22; font-weight:bold;",
          "⏳ Sitting out this hand"),
        p(style = "color:#888; font-size:12px;",
          "You joined mid-hand. You will be dealt in next hand.")
      )
    } else if (isTRUE(p$folded)) {
      p("You folded. Waiting for next hand.")
    } else {
      tagList(
        p("Waiting for:"),
        strong(textOutput("txt_cur_player", inline = TRUE))
      )
    }
  })
  
  output$ui_log <- renderUI({
    tick()
    logs <- game_state$action_log
    if (length(logs) == 0) return(p("No actions yet."))
    HTML(paste(htmltools::htmlEscape(logs), collapse = "<br>"))
  })
  
  output$txt_myname <- renderText({
    pid_str <- my_pid(); if (pid_str == "") return("")
    pid <- as.integer(pid_str); tick()
    if (pid > length(game_state$players)) return("")
    p_obj <- game_state$players[[pid]]
    s     <- p_obj$stats
    paste0(p_obj$name, "  |  Won: ", s$hands_won, "/", s$hands_played,
           "  Streak: ", s$current_streak)
  })
  
  # Banner แสดงสถานะ turn — แสดงบนปุ่ม action เสมอ
  output$ui_turn_banner <- renderUI({
    tick()
    pid_str <- my_pid(); if (pid_str == "") return(NULL)
    pid <- as.integer(pid_str)
    if (pid > length(game_state$players)) return(NULL)
    p <- game_state$players[[pid]]
    
    if (game_state$current_round == "showdown") {
      div(class = "turn-banner",
          style = "background:#8e44ad;",
          "SHOWDOWN")
    } else if (isTRUE(p$folded)) {
      div(class = "turn-banner",
          style = "background:#7f8c8d;",
          "YOU FOLDED")
    } else if (isTRUE(p$all_in)) {
      div(class = "turn-banner",
          style = "background:#e67e22;",
          "ALL-IN")
    } else if (isTRUE(game_state$current_turn == pid)) {
      div(class = "turn-banner",
          style = "background:#27ae60;",
          "YOUR TURN")
    } else {
      ct <- game_state$current_turn
      cur_name <- if (ct > 0L && ct <= length(game_state$players))
        game_state$players[[ct]]$name else "..."
      div(class = "turn-banner",
          style = "background:#2980b9;",
          paste0("Waiting for: ", cur_name))
    }
  })
}

shinyApp(ui = ui, server = server)