— Day 10: Adapter Array —
================
Fleur Kelpin
Dec 10, 2020

    library(tidyverse)
    input <- readr::read_csv("day10.txt", col_names = FALSE)[[1]]
    as_tibble(input)

    ## # A tibble: 102 x 1
    ##    value
    ##    <dbl>
    ##  1   153
    ##  2    17
    ##  3    45
    ##  4    57
    ##  5    16
    ##  6   147
    ##  7    39
    ##  8   121
    ##  9    75
    ## 10    70
    ## # … with 92 more rows

# Part 1

    input %>%
      sort() %>%
      embed(2) %>%
      { .[, 1] - .[, 2] } %>%
      { (sum(. == 1) + 1) * (sum(. == 3) + 1) }

    ## [1] 2346
