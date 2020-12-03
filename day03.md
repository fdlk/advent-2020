— Day 3: Toboggan Trajectory —
================
Fleur Kelpin
Dec 3, 2020

    library(tidyverse)
    input <- read_lines("day03.txt")

# Part 1

Start by **counting all the trees** you would encounter for the slope
right 3, down 1

    hasTreeOnRow <- function(rowNum, stepRight) {
      row <- input [[rowNum]]
      col <- ((stepRight * (rowNum - 1)) %% str_length(row)) + 1
      char <- str_sub(row, col, col)
      char == "#"
    }

    seq(along.with = input) %>%
      sapply(function(rowNum) {
        hasTreeOnRow(rowNum, 3)
      }) %>%
      sum()

    ## [1] 250

# Part 2

Determine the number of trees you would encounter if, for each of the
following slopes, you start at the top-left corner and traverse the map
all the way to the bottom:

-   Right 1, down 1.
-   Right 3, down 1. (This is the slope you already checked.)
-   Right 5, down 1.
-   Right 7, down 1.
-   Right 1, down 2.

**What do you get if you multiply together the number of trees
encountered on each of the listed slopes?**

    treesOnSlope <- function(stepRight, stepDown) {
      seq(from = 1, to = length(input), by = stepDown) %>%
        sapply(function(rowNum) {
          hasTreeOnRow(rowNum, stepRight / stepDown)
        }) %>%
        sum()
    }
    counts <- (c(
      treesOnSlope(1, 1),
      treesOnSlope(3, 1),
      treesOnSlope(5, 1),
      treesOnSlope(7, 1),
      treesOnSlope(1, 2)
    ))
    prod(counts)

    ## [1] 1592662500
