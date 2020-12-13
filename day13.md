— Day 13: Shuttle Search —
================
Fleur Kelpin
Dec 13, 2020

    library(tidyverse)
    input <- readLines("day13.txt")
    timestamp <- as.integer(input[[1]])
    buses <- tibble(id = input[[2]]) %>%
      separate_rows(id) %>%
      mutate(row = as.integer(row_number())) %>%
      filter(id != "x") %>%
      type_convert(cols(id = col_integer()))
    buses

    ## # A tibble: 9 x 2
    ##      id   row
    ##   <int> <int>
    ## 1    29     1
    ## 2    37    24
    ## 3   409    30
    ## 4    17    47
    ## 5    13    48
    ## 6    19    49
    ## 7    23    53
    ## 8   353    61
    ## 9    41   102

# Part 1

What is the ID of the earliest bus you can take to the airport
multiplied by the number of minutes you’ll need to wait for that bus?

    first <-
      buses %>%
      mutate(wait = id - timestamp %% id) %>%
      filter(wait == min(wait))
    first$id[[1]] * first$wait[[1]]

    ## [1] 222

# Part 2

What is the earliest timestamp such that all of the listed bus IDs
depart at offsets matching their positions in the list?

This comes down to solving a series of equations in the shape
`x ≡ a_i mod n_i`. We need an implementation of the chinese rest
theorem. Unfortunately, the one in the numbers package only works for
ordinary integers and works for all examples but overflows on the actual
puzzle input.

Hand-rolled implementation adapted from
<a href="https://codeforces.com/blog/entry/61290" class="uri">https://codeforces.com/blog/entry/61290</a>
does an extra division to prevent overflow.

    library(numbers)
    normalize <- function(x, mod) {
      x <- x %% mod
      if (x < 0) {
        x <- x + mod
      }
      x
    }
    chinese_no_overflow <- function(a, n) {
      ans <- a[[1]]
      lcm <- n[[1]]
      for (i in 2:length(a)) {
        pom <- extGCD(lcm, n[i])
        d <- pom[[1]]
        x1 <- pom[[2]]
        if ((a[i] - ans) %% d != 0) {
          print("no solutions")
          return(NA)
        }
        
        ans <- normalize(
          ans + x1 * (a[i] - ans) %/% d %% (n[i] %/% d) * lcm,
          (n[i] %/% d) * lcm
        )
        lcm <- LCM(lcm, n[i])
      }
      ans
    }
    options(digits=22)
    buses <- buses %>%
      mutate(mod = (id - row + 1) %% id)
    chinese_no_overflow(buses$mod, buses$id)

    ## [1] 408270049879073
