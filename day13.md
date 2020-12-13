— Day 13: Shuttle Search —
================
Fleur Kelpin
Dec 13, 2020

    library(tidyverse)
    input <- readLines("day13.txt")
    input

    ## [1] "1000511"                                                                                                                                                                                                               
    ## [2] "29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,409,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,19,x,x,x,23,x,x,x,x,x,x,x,353,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41"

# Part 1

    timestamp <- as.integer(input[[1]])
    buses <- tibble(id = input[[2]]) %>%
      separate_rows(id) %>%
      filter(id != "x") %>%
      type_convert(cols(id = col_double()))
    buses

    ## # A tibble: 9 x 1
    ##      id
    ##   <dbl>
    ## 1    29
    ## 2    37
    ## 3   409
    ## 4    17
    ## 5    13
    ## 6    19
    ## 7    23
    ## 8   353
    ## 9    41

What is the ID of the earliest bus you can take to the airport
multiplied by the number of minutes you’ll need to wait for that bus?

    first <-
      buses %>%
      mutate(wait = id - timestamp %% id) %>%
      filter(wait == min(wait))
    first$id[[1]] * first$wait[[1]]

    ## [1] 222
