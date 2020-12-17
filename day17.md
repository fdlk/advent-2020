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

    dim <- c(n + 2 * (steps+1),
             n + 2 * (steps+1),
             1 + 2 * (steps+1))
    state <- array(F, dim = dim)
    state[7+1:n,7+1:n,8] <- input

    for(i in 1:6) {
      next_state <- array(F, dim = dim)
      for(x in 2:dim[1]-1) {
        for(y in 2:dim[2]-1) {
          for(z in 2:dim[3]-1) {
            pop <- sum(state[x + (-1:1),
                             y + (-1:1),
                             z + (-1:1)])
            next_state[x,y,z] <- 
              (!state[x,y,z] & pop == 3) | 
              (state[x,y,z] & between(pop, 3, 4))
          }
        }
      }
      state <- next_state
    }
    sum(state)

    ## [1] 388

# Part 2

For some reason, your simulated results don’t match what the
experimental energy source engineers expected. Apparently, the pocket
dimension actually has four spatial dimensions, not three.

    dim <- c(n + 2 * (steps+1),
             n + 2 * (steps+1),
             1 + 2 * (steps+1),
             1 + 2 * (steps+1))
    state <- array(F, dim = dim)
    state[7+(1:n),7+(1:n),8,8] <- input

    for(i in 1:6) {
      next_state <- array(F, dim = dim)
      for(x in 2:dim[1]-1) {
        for(y in 2:dim[2]-1) {
          for(z in 2:dim[3]-1) {
            for(w in 2:dim[4]-1) {
              pop <- sum(state[x + (-1:1),
                               y + (-1:1),
                               z + (-1:1),
                               w + (-1:1)])
              next_state[x,y,z,w] <- 
                (!state[x,y,z,w] & pop == 3) | 
                (state[x,y,z,w] & between(pop, 3, 4))
            }
          }
        }
      }
      state <- next_state
    }
    sum(state)

    ## [1] 2280
