— Day 11: Seating System —
================
Fleur Kelpin
Dec 11, 2020

    library(tidyverse)
    library(Matrix)
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
      d_le <- cbind(d[, -1, drop = FALSE], 0)
      d_re <- cbind(0, d[, -ncol, drop = FALSE])
      d_lu <- cbind(d_eu[, -1, drop = FALSE], 0)
      d_ru <- cbind(0, d_eu[, -ncol, drop = FALSE])
      d_ld <- cbind(d_ed[, -1, drop = FALSE], 0)
      d_rd <- cbind(0, d_ed[, -ncol, drop = FALSE])
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

# Part 2

As soon as people start to arrive, you realize your mistake. People
don’t just care about adjacent seats - they care about the first seat
they can see in each of those eight directions!

Now, instead of considering just the eight immediately adjacent seats,
consider the first seat in each of those eight directions.

I realise my mistake: to do advent of code in R!

R likes vectors, so let’s number the seats, then the state becomes a
boolean vector.

    seat_coords <- as_tibble(which(seats, arr.ind = TRUE)) %>%
      mutate(number = row_number())

    num_seats <- nrow(seat_coords)

    seat_num <- function(i, j) {
      filter(seat_coords, row == i & col == j)$number
    }

    seat_coords

    ## # A tibble: 6,823 x 3
    ##      row   col number
    ##    <int> <int>  <int>
    ##  1     1     1      1
    ##  2     2     1      2
    ##  3     3     1      3
    ##  4     4     1      4
    ##  5     5     1      5
    ##  6     6     1      6
    ##  7     7     1      7
    ##  8     8     1      8
    ##  9    10     1      9
    ## 10    11     1     10
    ## # … with 6,813 more rows

The only row/column information we need to retain is the visible
neighbors. Let’s find the nearest seat in a direction:

    neighbor_seat <- function(i, j, di, dj) {
      while (((i <- i + di) %in% 1:nrow(seats)) &
        ((j <- j + dj) %in% 1:ncol(seats))) {
        if (seats[i, j]) {
          return(seat_num(i, j))
        }
      }
      NA
    }
    seat_coords[neighbor_seat(1, 1, 1, 1), ]

    ## # A tibble: 1 x 3
    ##     row   col number
    ##   <int> <int>  <int>
    ## 1     2     2     84

Then we can turn the counting of the neighbors into a matrix
multiplication. The sparse neighbor matrix, when multiplied with the
seat vector, returns a vector with the number of occupied neighbors for
each seat.

    neighbors <- function(i, j) {
      n <- c(
        neighbor_seat(i, j, -1, -1),
        neighbor_seat(i, j, 0, -1),
        neighbor_seat(i, j, 1, -1),
        neighbor_seat(i, j, 1, 0),
        neighbor_seat(i, j, 1, 1),
        neighbor_seat(i, j, 0, 1),
        neighbor_seat(i, j, -1, 1),
        neighbor_seat(i, j, -1, 0)
      )
      n <- n[!is.na(n)]
      sparseMatrix(
        i = rep(seat_num(i, j), length(n)),
        j = n,
        dims = c(num_seats, num_seats)
      )
    }

This bit is still loopy and it takes a couple of minutes to fill the
matrix.

    neighbor_matrix <- sparseMatrix(c(), c(), dims = c(num_seats, num_seats))
    for (number in 1:num_seats) {
      row <- seat_coords$row[[number]]
      col <- seat_coords$col[[number]]
      neighbor_matrix <- neighbor_matrix + neighbors(row, col)
    }
    dim(neighbor_matrix)

    ## [1] 6823 6823

But iterating is a lightning fast vector operation now!

    life_step <- function(d) {
      pop <- (neighbor_matrix %*% d)[, 1]
      d <- (!d & pop == 0) | (d & pop < 5)
    }
    state <- rep(0, num_seats)
    next_state <- life_step(state)
    while (any(next_state != state)) {
      state <- next_state
      next_state <- life_step(state)
    }
    sum(state)

    ## [1] 1914
