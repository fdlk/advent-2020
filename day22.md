— Day 22: Crab Combat —
================
Fleur Kelpin
Dec 22, 2020

    library(tidyverse)
    input_player1 <- read_csv("day22-player1.txt", col_names = c("card"))$card
    input_player2 <- read_csv("day22-player2.txt", col_names = c("card"))$card
    input_player1

    ##  [1] 10 39 16 32  5 46 47 45 48 26 36 27 24 37 49 25 30 13 23  1  9  3 31 14  4

    input_player2

    ##  [1]  2 15 29 41 11 21  8 44 38 19 12 20 40 17 22 35 34 42 50  6 33  7 18 28 43

# Part 1

> Before the game starts, split the cards so each player has their own
> deck (your puzzle input). Then, the game consists of a series of
> rounds: both players draw their top card, and the player with the
> higher-valued card wins the round. The winner keeps both cards,
> placing them on the bottom of their own deck so that the winner’s card
> is above the other card. If this causes a player to have all of the
> cards, they win, and the game ends.

> Once the game ends, you can calculate the winning player’s score. The
> bottom card in their deck is worth the value of the card multiplied by
> 1, the second-from-the-bottom card is worth the value of the card
> multiplied by 2, and so on. Play the small crab in a game of Combat
> using the two decks you just dealt. What is the winning player’s
> score?

    p1 <- input_player1
    p2 <- input_player2
    num_cards <- length(input_player1) + length(input_player2)
    while (between(length(p1), 1, num_cards - 1)) {
      # print(list(p1 = p1, p2 = p2))
      trick <- c(p1[[1]], p2[[1]])
      p1 <- tail(p1, n = -1)
      p2 <- tail(p2, n = -1)
      sorted <- sort(trick, decreasing = T)
      if (all(trick == sorted)) {
        p1 <- append(p1, sorted)
      } else {
        p2 <- append(p2, sorted)
      }
    }
    winning_hand <- c(p1, p2)
    sum(winning_hand * (num_cards:1))

    ## [1] 31455

    library(hash)

    ## hash-2.2.6.1 provided by Decision Patterns

    game <- function(p1, p2) {
      seen <- hash()
      num_cards <- length(p1) + length(p2)
      i <- 1
      while (between(length(p1), 1, num_cards - 1)) {
        #print(list(turn = i, p1 = p1, p2 = p2))
        i <- i + 1
        key <- paste(c(p1, NA, p2), collapse = "_")
        if (has.key(key, seen)) {
          return(list(winner = 1, hand = p1))
        }
        seen[key] <- TRUE
        trick <- c(p1[[1]], p2[[1]])
        p1 <- tail(p1, n = -1)
        p2 <- tail(p2, n = -1)
        if (length(p1) >= trick[[1]] & length(p2) >= trick[[2]]) {
          winner <- game(p1[1:trick[[1]]], p2[1:trick[[2]]])$winner
        } else {
          winner <- if (trick[[1]] > trick[[2]]) 1 else 2
        }
        if (winner == 1) {
          p1 <- append(p1, trick)
        } else {
          p2 <- append(p2, rev(trick))
        }
      }
      if (length(p2) == 0) {
        list(winner = 1, hand = p1)
      } else {
        list(winner = 2, hand = p2)
      }
    }

    game(c(43, 19), c(2, 29,14))

    ## $winner
    ## [1] 1
    ## 
    ## $hand
    ## [1] 43 19

    game(c(9, 2, 6, 3, 1), c(5, 8, 4, 7, 10))

    ## $winner
    ## [1] 2
    ## 
    ## $hand
    ##  [1]  7  5  6  2  4  1 10  8  9  3

    result <- game(input_player1, input_player2)
    result$hand

    ##  [1]  1  6 49 29 30 18 32 25 35 20 21  3 46 43 45  8 16  7 42 24 39 19 48 47  5
    ## [26]  4 44 15 22  2 38 31 34 28 41 17 37 12 27 14 26 10 50 23 36 11 40 13 33  9

    sum(result$hand * (length(result$hand):1))

    ## [1] 32528
