— Day 15: Rambunctious Recitation —
================
Fleur Kelpin
Dec 15, 2020

    library(tidyverse)
    input <- tibble(number = readLines("day15.txt")) %>%
      separate_rows(number, sep = ",", convert = TRUE)
    input

    ## # A tibble: 7 x 1
    ##   number
    ##    <int>
    ## 1      6
    ## 2      4
    ## 3     12
    ## 4      1
    ## 5     20
    ## 6      0
    ## 7     16

# Part 1

In this game, the players take turns saying numbers. They begin by
taking turns reading from a list of starting numbers (your puzzle
input). Then, each turn consists of considering the most recently spoken
number:

-   If that was the first time the number has been spoken, the current
    player says 0.
-   Otherwise, the number had been spoken before; the current player
    announces how many turns apart the number is from when it was
    previously spoken.

So, after the starting numbers, each turn results in that player
speaking aloud either 0 (if the last number is new) or an age (if the
last number is a repeat).

    game <- c(input$number)
    while (length(game) < 2020) {
      occurred_last <- tail(which(game == last(game)), 2)
      game <- append(
        game,
        if (length(occurred_last) == 1) {
          0
        } else {
          occurred_last[[2]] - occurred_last[[1]]
        }
      )
    }

    last(game)

    ## [1] 475

# Part 2

Given your starting numbers, what will be the 30000000th number spoken?

    library(hash)

    ## hash-2.2.6.1 provided by Decision Patterns

    game <- hash()
    for (i in 1:(length(input$number) - 1)) {
      number <- input$number[[i]]
      game[as.character(number)] <- i
    }
    last_number <- last(input$number)
    for (i in (length(input$number) + 1):30000000) {
      occurred_last <- game[[as.character(last_number)]]
      game[[as.character(last_number)]] <- i - 1
      last_number <- if (is.null(occurred_last)) {
        0
      } else {
        i - 1 - occurred_last
      }
    }
    last_number

    ## [1] 11261
