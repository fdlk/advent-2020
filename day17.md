— Day 17: Conway Cubes —
================
Fleur Kelpin
Dec 17, 2020

    library(tidyverse)
    n <- 8
    steps <- 6
    input <- readLines("day17.txt") %>%
      str_split("") %>%
      unlist() %>%
      map_lgl(~ . == "#") %>%
      matrix(nrow = n, byrow = T)

# Part 1

Starting with your given initial configuration, simulate six cycles. How
many cubes are left in the active state after the sixth cycle?

    dim <- c(
      n + 2 * (steps + 1),
      n + 2 * (steps + 1),
      1 + 2 * (steps + 1)
    )
    state <- array(F, dim = dim)
    state[7 + 1:n, 7 + 1:n, 8] <- input
    ds <- expand.grid(dx = -1:1, dy = -1:1, dz = -1:1)
    innerxy <- 2:(dim[[1]] - 1)
    innerzw <- 2:(dim[[3]] - 1)

    for (i in 1:6) {
      pop <- reduce(1:nrow(ds),
        function(arr, index) {
          arr + state[
            innerxy + ds$dx[[index]],
            innerxy + ds$dy[[index]],
            innerzw + ds$dz[[index]]
          ]
        },
        .init = array(0, dim = c(
          2 * steps + n,
          2 * steps + n,
          2 * steps + 1
        ))
      )
      state[innerxy, innerxy, innerzw] <-
        (!state[innerxy, innerxy, innerzw] & pop == 3) |
          (state[innerxy, innerxy, innerzw] & between(pop, 3, 4))
    }
    sum(state)

    ## [1] 388

# Part 2

For some reason, your simulated results don’t match what the
experimental energy source engineers expected. Apparently, the pocket
dimension actually has four spatial dimensions, not three.

    dim <- c(
      n + 2 * (steps + 1),
      n + 2 * (steps + 1),
      1 + 2 * (steps + 1),
      1 + 2 * (steps + 1)
    )
    state <- array(F, dim = dim)
    state[7 + (1:n), 7 + (1:n), 8, 8] <- input
    ds <- expand.grid(dx = -1:1, dy = -1:1, dz = -1:1, dw = -1:1)

    for (i in 1:6) {
      pop <- reduce(1:nrow(ds),
        function(arr, index) {
          arr + state[
            innerxy + ds$dx[[index]],
            innerxy + ds$dy[[index]],
            innerzw + ds$dz[[index]],
            innerzw + ds$dw[[index]]
          ]
        },
        .init = array(0, dim = c(
          2 * steps + n,
          2 * steps + n,
          2 * steps + 1,
          2 * steps + 1
        ))
      )
      state[innerxy, innerxy, innerzw, innerzw] <-
        (!state[innerxy, innerxy, innerzw, innerzw] & pop == 3) |
          (state[innerxy, innerxy, innerzw, innerzw] & between(pop, 3, 4))
    }
    sum(state)

    ## [1] 2280
