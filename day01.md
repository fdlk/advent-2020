— Day 1: Report Repair —
================
Fleur Kelpin
Dec 1, 2020

    library(readr)
    input <- read.csv("day01.txt")[, 1]

# Part 1

> Find the two entries that sum to 2020 and then multiply those two
> numbers together.

    x <- (c(input, 2020 - input))
    duplicates <- x[duplicated(x)]
    prod(duplicates)

    ## [1] 786811

# Part 2

> What is the product of the three entries that sum to 2020?

    combinations <- combn(input, 3)
    entries <- combinations[, colSums(combinations) == 2020]
    prod(entries)

    ## [1] 199068980
