# ============================================================
# POKER LOGIC  (cards stored as e.g. "As", "Kh", "Td", "2c")
# ============================================================

RANKS      <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")
SUITS      <- c("s","h","d","c")
RANK_VAL   <- setNames(2:14, RANKS)
SUIT_SYM   <- c(s="\u2660", h="\u2665", d="\u2666", c="\u2663")
HAND_NAMES <- c("High Card","One Pair","Two Pair","Three of a Kind",
                "Straight","Flush","Full House","Four of a Kind",
                "Straight Flush","Royal Flush")

create_deck <- function() {
  paste0(rep(RANKS, each = 4), rep(SUITS, 13))
}

card_value <- function(card) {
  unname(RANK_VAL[substr(card, 1, nchar(card) - 1)])
}

card_suit <- function(card) {
  substr(card, nchar(card), nchar(card))
}

format_card <- function(card) {
  r <- substr(card, 1, nchar(card) - 1)
  s <- substr(card, nchar(card), nchar(card))
  paste0(r, SUIT_SYM[s])
}

format_cards <- function(cards) {
  paste(vapply(cards, format_card, character(1)), collapse = " ")
}

# Returns list(rank=0..9, name, tiebreak)
evaluate_hand <- function(cards) {
  if (length(cards) < 5) {
    return(list(rank = -1, name = "Incomplete", tiebreak = integer(0)))
  }

  vals  <- vapply(cards, card_value, numeric(1))
  suits <- vapply(cards, card_suit,  character(1))

  rank_cnts <- sort(table(vals), decreasing = TRUE)
  suit_cnts <- table(suits)
  cnts      <- as.integer(rank_cnts)

  # --- Flush ---
  fs <- which(suit_cnts >= 5)
  is_flush  <- length(fs) > 0
  flush_suit <- if (is_flush) names(suit_cnts)[fs[1]] else NULL

  # --- Straight flush ---
  is_sf <- FALSE; sf_high <- 0
  if (is_flush) {
    fv <- sort(unique(vals[suits == flush_suit]), decreasing = TRUE)
    if (length(fv) >= 5) {
      for (i in seq_len(length(fv) - 4)) {
        if (all(diff(fv[i:(i+4)]) == -1)) { is_sf <- TRUE; sf_high <- fv[i]; break }
      }
    }
    if (!is_sf && all(c(14, 5, 4, 3, 2) %in% vals[suits == flush_suit])) {
      is_sf <- TRUE; sf_high <- 5
    }
  }

  # --- Straight ---
  uv     <- sort(unique(vals), decreasing = TRUE)
  is_str <- FALSE; str_high <- 0
  if (length(uv) >= 5) {
    for (i in seq_len(length(uv) - 4)) {
      if (all(diff(uv[i:(i+4)]) == -1)) { is_str <- TRUE; str_high <- uv[i]; break }
    }
  }
  if (!is_str && all(c(14, 5, 4, 3, 2) %in% vals)) { is_str <- TRUE; str_high <- 5 }

  tb <- sort(vals, decreasing = TRUE)   # general tiebreak

  if (is_sf && sf_high == 14)                      return(list(rank=9, name="Royal Flush",     tiebreak=c(14)))
  if (is_sf)                                        return(list(rank=8, name="Straight Flush",  tiebreak=c(sf_high)))
  if (cnts[1] == 4)                                 return(list(rank=7, name="Four of a Kind",  tiebreak=tb))
  if (cnts[1] == 3 && length(cnts)>=2 && cnts[2]>=2) return(list(rank=6, name="Full House",   tiebreak=tb))
  if (is_flush) {
    fv5 <- sort(vals[suits==flush_suit], decreasing=TRUE)[1:5]
    return(list(rank=5, name="Flush", tiebreak=fv5))
  }
  if (is_str)                                       return(list(rank=4, name="Straight",        tiebreak=c(str_high)))
  if (cnts[1] == 3)                                 return(list(rank=3, name="Three of a Kind", tiebreak=tb))
  if (cnts[1] == 2 && length(cnts)>=2 && cnts[2]==2) return(list(rank=2, name="Two Pair",      tiebreak=tb))
  if (cnts[1] == 2)                                 return(list(rank=1, name="One Pair",        tiebreak=tb))
  return(list(rank=0, name="High Card", tiebreak=tb))
}

# Returns: 1 = cards1 wins, -1 = cards2 wins, 0 = tie
compare_hands <- function(cards1, cards2) {
  h1 <- evaluate_hand(cards1)
  h2 <- evaluate_hand(cards2)
  if (h1$rank > h2$rank) return(1)
  if (h1$rank < h2$rank) return(-1)
  n <- min(length(h1$tiebreak), length(h2$tiebreak))
  for (i in seq_len(n)) {
    if (h1$tiebreak[i] > h2$tiebreak[i]) return(1)
    if (h1$tiebreak[i] < h2$tiebreak[i]) return(-1)
  }
  return(0)
}
