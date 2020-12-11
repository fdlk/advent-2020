— Day 11: Seating System —
================
Fleur Kelpin
Dec 11, 2020

    library(tidyverse)
    input <- read_csv(file = "day11.txt", col_names = c("seat"))
    t <- input %>%
      separate_rows(seat, sep = "", convert = TRUE) %>%
      drop_na()
    seats <- matrix(data = t$seat, nrow = nrow(input), byrow = TRUE)
    seats

    ##        [,1]  [,2] [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
    ##  [1,]  TRUE FALSE TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE
    ##  [2,]  TRUE  TRUE TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
    ##  [3,]  TRUE FALSE TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE
    ##  [4,]  TRUE  TRUE TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE
    ##  [5,]  TRUE FALSE TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE
    ##  [6,]  TRUE FALSE TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
    ##  [7,] FALSE FALSE TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
    ##  [8,]  TRUE  TRUE TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ##  [9,]  TRUE FALSE TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
    ## [10,]  TRUE FALSE TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE

# Part 1
