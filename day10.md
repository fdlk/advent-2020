— Day 10: Adapter Array —
================
Fleur Kelpin
Dec 10, 2020

    library(tidyverse)
    input <- readr::read_csv("day10.txt", col_names = FALSE)[[1]] %>%
      prepend(0) %>%
      {append(., max(.) + 3)} %>%
      sort()
      
    as_tibble(input)

    ## # A tibble: 104 x 1
    ##    value
    ##    <dbl>
    ##  1     0
    ##  2     1
    ##  3     2
    ##  4     3
    ##  5     4
    ##  6     7
    ##  7    10
    ##  8    11
    ##  9    12
    ## 10    15
    ## # … with 94 more rows

# Part 1

    input %>%
      embed(2) %>%
      { .[, 1] - .[, 2] } %>%
      { sum(. == 1) * sum(. == 3) }

    ## [1] 2346

# Part 2

    x <- tibble(jolt = input, ways=as.numeric(NA))
    x[1, 'ways'] = 1

    while((index <- detect_index(x$ways, is.na)) > 0 ){
      joltage <- x[index, 'jolt']
      blah <- filter(x, between(jolt, joltage-3, joltage-1))
      x[index, 'ways'] = sum(blah$ways)
    }
    options(digits = 15)
    max(x$ways)

    ## [1] 6044831973376
