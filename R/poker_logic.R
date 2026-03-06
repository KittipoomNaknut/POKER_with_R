create_deck <- function() {
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  suits <- c("♠", "♥", "♦", "♣")
  
  deck <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)
  deck$value <- match(deck$rank, ranks)
  deck$card_id <- paste0(deck$rank, deck$suit)
  
  return(deck)
}

evaluate_hand <- function(cards) {
  if (length(cards) < 5) {
    return(list(hand = "Incomplete", rank = 0, desc = "Need 5+ cards"))
  }
  
  deck <- create_deck()
  hand_data <- deck[deck$card_id %in% cards, ]
  
  if (nrow(hand_data) < 5) {
    return(list(hand = "Invalid", rank = 0, desc = "Invalid cards"))
  }
  
  # Sort by value
  hand_data <- hand_data[order(-hand_data$value), ]
  
  ranks <- hand_data$rank
  suits <- hand_data$suit
  values <- hand_data$value
  
  # Count frequencies
  rank_counts <- table(ranks)
  suit_counts <- table(suits)
  
  # Check flush (5+ same suit)
  is_flush <- any(suit_counts >= 5)
  flush_suit <- NULL
  if (is_flush) {
    flush_suit <- names(suit_counts[suit_counts >= 5])[1]
  }

  # Check straight flush: 5 consecutive cards of the SAME suit
  is_straight_flush <- FALSE
  straight_flush_high <- 0

  if (is_flush && !is.null(flush_suit)) {
    flush_vals <- sort(unique(hand_data$value[hand_data$suit == flush_suit]),
                       decreasing = TRUE)

    if (length(flush_vals) >= 5) {
      for (i in 1:(length(flush_vals) - 4)) {
        if (all(diff(flush_vals[i:(i + 4)]) == -1)) {
          is_straight_flush <- TRUE
          straight_flush_high <- flush_vals[i]
          break
        }
      }
    }

    # Wheel: A-2-3-4-5 straight flush
    if (!is_straight_flush &&
        all(c(14, 5, 4, 3, 2) %in% hand_data$value[hand_data$suit == flush_suit])) {
      is_straight_flush <- TRUE
      straight_flush_high <- 5
    }
  }

  # Check straight (across any suits)
  is_straight <- FALSE
  straight_high <- 0

  unique_values <- sort(unique(values), decreasing = TRUE)

  # Normal straights
  if (length(unique_values) >= 5) {
    for (i in 1:(length(unique_values) - 4)) {
      if (all(diff(unique_values[i:(i+4)]) == -1)) {
        is_straight <- TRUE
        straight_high <- unique_values[i]
        break
      }
    }
  }

  # Special case: A-2-3-4-5 (wheel)
  if (!is_straight && all(c(14, 5, 4, 3, 2) %in% values)) {
    is_straight <- TRUE
    straight_high <- 5
  }

  # Hand type determination
  max_count <- max(rank_counts)
  num_pairs <- sum(rank_counts == 2)
  num_trips <- sum(rank_counts == 3)
  num_quads <- sum(rank_counts == 4)

  # Royal Flush (straight flush with high card = Ace)
  if (is_straight_flush && straight_flush_high == 14) {
    return(list(
      hand = "Royal Flush",
      rank = 10,
      desc = "A-K-Q-J-10 suited"
    ))
  }

  # Straight Flush
  if (is_straight_flush) {
    return(list(
      hand = "Straight Flush",
      rank = 9,
      desc = paste("Straight Flush,", straight_flush_high, "high")
    ))
  }
  
  # Four of a Kind
  if (num_quads >= 1) {
    quad_rank <- names(rank_counts[rank_counts == 4])[1]
    return(list(
      hand = "Four of a Kind",
      rank = 8,
      desc = paste("Quad", quad_rank, "'s")
    ))
  }
  
  # Full House
  if (num_trips >= 1 && num_pairs >= 1) {
    trip_rank <- names(rank_counts[rank_counts == 3])[1]
    pair_rank <- names(rank_counts[rank_counts == 2])[1]
    return(list(
      hand = "Full House",
      rank = 7,
      desc = paste(trip_rank, "'s full of", pair_rank, "'s")
    ))
  }
  
  # Flush
  if (is_flush) {
    return(list(
      hand = "Flush",
      rank = 6,
      desc = paste(flush_suit, "Flush")
    ))
  }
  
  # Straight
  if (is_straight) {
    return(list(
      hand = "Straight",
      rank = 5,
      desc = paste("Straight,", straight_high, "high")
    ))
  }
  
  # Three of a Kind
  if (num_trips >= 1) {
    trip_rank <- names(rank_counts[rank_counts == 3])[1]
    return(list(
      hand = "Three of a Kind",
      rank = 4,
      desc = paste("Trip", trip_rank, "'s")
    ))
  }
  
  # Two Pair
  if (num_pairs >= 2) {
    pair_ranks <- names(rank_counts[rank_counts == 2])
    return(list(
      hand = "Two Pair",
      rank = 3,
      desc = paste(pair_ranks[1], "'s &", pair_ranks[2], "'s")
    ))
  }
  
  # One Pair
  if (num_pairs >= 1) {
    pair_rank <- names(rank_counts[rank_counts == 2])[1]
    return(list(
      hand = "One Pair",
      rank = 2,
      desc = paste("Pair of", pair_rank, "'s")
    ))
  }
  
  # High Card
  high_card <- ranks[1]
  return(list(
    hand = "High Card",
    rank = 1,
    desc = paste(high_card, "high")
  ))
}