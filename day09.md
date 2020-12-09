— Day 9: Encoding Error —
================
Fleur Kelpin
Dec 9, 2020

    library(tidyverse)
    input <- readr::read_csv("day09.txt")[[1]]
    as_tibble(input)

    ## # A tibble: 999 x 1
    ##    value
    ##    <dbl>
    ##  1     3
    ##  2    36
    ##  3    14
    ##  4    50
    ##  5     8
    ##  6     2
    ##  7    44
    ##  8    30
    ##  9    37
    ## 10    20
    ## # … with 989 more rows

# Part 1

The first step of attacking the weakness in the XMAS data is to find the
first number in the list (after the preamble) which is not the sum of
two of the 25 numbers before it. **What is the first number that does
not have this property?**

    window <- 25

    invalid <- function(i) {
      number <- input[[i]]
      preamble <- input[(i - window):i - 1]
      !any(preamble %in% (number - preamble))
    }

    part1 <- seq(window + 1, length(input)) %>%
      detect_index(invalid) %>%
      { input[[window + .]] }

    part1

    ## [1] 41682220

# Part 2

The final step in breaking the XMAS encryption relies on the invalid
number you just found: you must find a contiguous set of at least two
numbers in your list which sum to the invalid number from step 1.

To find the encryption weakness, add together the smallest and largest
number in this contiguous range.

**What is the encryption weakness in your XMAS-encrypted list of
numbers?**

    contiguous_range <- function(start, window) {
      input[start:(start + window - 1)]
    }

    find_start <- function(window) {
      detect_index(
        seq(length(input) - window),
        ~ sum(contiguous_range(.x, window)) == part1
      )
    }

    window <- detect(seq(2, length(input)), ~ find_start(.) > 0)
    start <- find_start(window)
    contiguous_range(start, window) %>% { min(.) + max(.) }

    ## [1] 5388976
