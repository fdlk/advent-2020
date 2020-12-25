— Day 25: Combo Breaker —
================
Fleur Kelpin
Dec 25, 2020

    library(tidyverse)
    library(numbers)
    input <- read_csv("day25.txt", col_names = "key")
    input

    ## # A tibble: 2 x 1
    ##        key
    ##      <dbl>
    ## 1  5290733
    ## 2 15231938

# Part 1

    transform_subject <- function (subject, loop) {
      modpower(subject, loop, 20201227)
    }

    find_loop_size <- function (key) {
      x <- 1
      loop <- 0
      repeat {
        loop <- loop + 1
        x <- (x * 7) %% 20201227
        if (x == key) {
          return(loop)
        }
      }
    }
    transform_subject(input$key[[1]], find_loop_size(input$key[[2]]))

    ## [1] 6198540
