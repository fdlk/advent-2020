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
    dim(seats)

    ## [1] 90 92

# Part 1

Now, you just need to model the people who will be arriving shortly.
Fortunately, people are entirely predictable and always follow a simple
set of rules. All decisions are based on the number of occupied seats
adjacent to a given seat (one of the eight positions immediately up,
down, left, right, or diagonal from the seat). The following rules are
applied to every seat simultaneously:

-   If a seat is empty (L) and there are no occupied seats adjacent to
    it, the seat becomes occupied.
-   If a seat is occupied (\#) and four or more seats adjacent to it are
    also occupied, the seat becomes empty.
-   Otherwise, the seat’s state does not change.

[Game of
life](https://win-vector.com/2018/10/28/conways-game-of-life-in-r-or-on-the-importance-of-vectorizing-your-r-code/)
with a mask. R is supposed to be good with matrices, so this should be a
breeze.

    life_step <- function(seats, d) {
      # form the neighboring sums
      nrow <- dim(d)[[1]]
      ncol <- dim(d)[[2]]
      d_eu <- rbind(d[-1, , drop = FALSE], 0)
      d_ed <- rbind(0, d[-nrow, , drop = FALSE])
      d_le <- cbind(d[ , -1, drop = FALSE], 0)
      d_re <- cbind(0, d[ , -ncol, drop = FALSE])
      d_lu <- cbind(d_eu[ , -1, drop = FALSE], 0)
      d_ru <- cbind(0, d_eu[ , -ncol, drop = FALSE])
      d_ld <- cbind(d_ed[ , -1, drop = FALSE], 0)
      d_rd <- cbind(0, d_ed[ , -ncol, drop = FALSE])
      pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd
      d <- (!d & pop == 0) | (d & pop < 4)
      d & seats
    }
    state <- seats & FALSE
    next_state <- life_step(seats, state)
    while (any(next_state != state)) {
      state <- next_state
      next_state <- life_step(seats, state)
    }
    sum(state)

    ## [1] 2126
