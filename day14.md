— Day 14: Docking Data —
================
Fleur Kelpin
Dec 14, 2020

    library(tidyverse)
    input <- tibble(line = readLines("day14.txt")) %>%
      extract(line, "mask", "mask = ([01X]{36})", remove = FALSE) %>%
      extract(line, c("address", "value"), "mem\\[(\\d+)\\] = (\\d+)", convert = TRUE)
    input

    ## # A tibble: 570 x 3
    ##    address     value mask                                
    ##      <int>     <int> <chr>                               
    ##  1      NA        NA 0111X10100100X1111X10010X000X1000001
    ##  2   50907 468673978 <NA>                                
    ##  3   22295   3337449 <NA>                                
    ##  4   58474  56418393 <NA>                                
    ##  5   15362    243184 <NA>                                
    ##  6   65089 110688658 <NA>                                
    ##  7      NA        NA 010X010XX110X01X01X10X001001011X110X
    ##  8   21952    950257 <NA>                                
    ##  9   44861 522064487 <NA>                                
    ## 10   38886  28536885 <NA>                                
    ## # … with 560 more rows

# Part 1

Execute the initialization program. What is the sum of all values left
in memory after it completes?

    i2b <- function(value) {
      result <- logical(36)
      index <- 36
      while (value > 0) {
        result[index] <- value %% 2 == 1
        value <- value %/% 2
        index <- index - 1
      }
      result
    }

    b2d <- function(bits) {
      result <- 0
      for (index in 1:36) {
        if (bits[[index]]) {
          result <- result + 2**(36 - index)
        }
      }
      result
    }

    apply_mask <- function(value, mask) {
      bits <- i2b(value)
      mask <- unlist(str_split(mask, ""))
      masked_bits <-
        map_lgl(1:36, function(index) {
          switch(mask[index],
            "1" = TRUE,
            "0" = FALSE,
            "X" = bits[index]
          )
        })
      b2d(masked_bits)
    }

    mem <- double()

    for (i in 1:nrow(input)) {
      if (!is.na(input$mask[[i]])) {
        mask <- input$mask[[i]]
      } else {
        mem[input$address[[i]]] <- apply_mask(input$value[[i]], mask)
      }
    }

    options(digits = 22)
    sum(mem, na.rm = TRUE)

    ## [1] 9628746976360
