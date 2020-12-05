— Day 1: Report Repair —
================
Fleur Kelpin
Dec 1, 2020

    library(tidyverse)
    input <- read.csv("day01.txt")[, 1]

# Part 1

> Find the two entries that sum to 2020 and then multiply those two
> numbers together.

    input %>%
      c(., 2020 - .) %>%
      .[duplicated(.)] %>%
      prod()

    ## [1] 786811

# Part 2

> What is the product of the three entries that sum to 2020?

    input %>%
      combn(3) %>%
      .[,colSums(.) == 2020] %>%
      prod()

    ## [1] 199068980
