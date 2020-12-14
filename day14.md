— Day 14: Docking Data —
================
Fleur Kelpin
Dec 14, 2020

    library(tidyverse)
    input <- tibble(line = readLines("day14.txt")) %>%
      extract(line, "mask", "mask = ([01X]{36})", remove = FALSE) %>%
      extract(line, c("address", "value"), "mem\\[(\\d+)\\] = (\\d+)",
        convert = TRUE
      )
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

The initialization program (your puzzle input) can either update the
bitmask or write a value to memory. Values and memory addresses are both
36-bit unsigned integers. For example, ignoring bitmasks for a moment, a
line like `mem[8] = 11` would write the value 11 to memory address 8.

The bitmask is always given as a string of 36 bits, written with the
most significant bit (representing 2^35) on the left and the least
significant bit (2^0, that is, the 1s bit) on the right. The current
bitmask is applied to values immediately before they are written to
memory: a 0 or 1 overwrites the corresponding bit in the value, while an
X leaves the bit in the value unchanged.

Execute the initialization program. What is the sum of all values left
in memory after it completes?

    i2b <- function(value) {
      value %/% (2**(35:0)) %% 2 == 1
    }

    b2d <- function(bits) {
      sum(2**(36 - which(bits)))
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

# Part 2

For some reason, the sea port’s computer system still can’t communicate
with your ferry’s docking program. It must be using version 2 of the
decoder chip!

A version 2 decoder chip doesn’t modify the values being written at all.
Instead, it acts as a memory address decoder. Immediately before a value
is written to memory, each bit in the bitmask modifies the corresponding
bit of the destination memory address in the following way:

-   If the bitmask bit is 0, the corresponding memory address bit is
    unchanged.
-   If the bitmask bit is 1, the corresponding memory address bit is
    overwritten with 1.
-   If the bitmask bit is X, the corresponding memory address bit is
    floating.

A floating bit is not connected to anything and instead fluctuates
unpredictably. In practice, this means the floating bits will take on
all possible values, potentially causing many memory addresses to be
written all at once!

    library(hash)

    ## hash-2.2.6.1 provided by Decision Patterns

    expand_masks <- function(mask) {
      if (str_detect(mask, "X")) {
        c(
          expand_masks(str_replace(mask, "X", "0")),
          expand_masks(str_replace(mask, "X", "1"))
        )
      } else {
        b2d(unlist(str_split(mask, "")) == "1")
      }
    }
    apply_mask_v2 <- function(value, mask) {
      bits <- i2b(value)
      chars <- unlist(str_split(mask, ""))
      for (i in str_which("0", chars)) {
        substr(mask, i, i) <- if (bits[[i]]) "1" else "0"
      }
      expand_masks(mask)
    }
    apply_mask_v2(42, "000000000000000000000000000000X1001X")

    ## [1] 26 27 58 59

    mem <- hash()
    for (i in 1:nrow(input)) {
      if (!is.na(input$mask[[i]])) {
        mask <- input$mask[[i]]
      } else {
        addresses <- apply_mask_v2(input$address[[i]], mask)
        mem[addresses] <- input$value[[i]]
      }
    }
    sum(values(mem))

    ## [1] 4574598714592
