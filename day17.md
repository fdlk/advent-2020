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
    input

    ##       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]
    ## [1,] FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
    ## [2,]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
    ## [3,]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## [4,]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## [5,]  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE
    ## [6,]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## [7,] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
    ## [8,]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE

# Part 1

Starting with your given initial configuration, simulate six cycles. How
many cubes are left in the active state after the sixth cycle?

    dim <- c(n + 2 * steps, n + 2 * steps, 1 + 2 * steps)
    state <- array(0, dim = dim)
    for(x in 1:n) {
      for(y in 1:n) {
        state[x+6,y+6,7] <- input[x,y]
      }
    }

    neighbors <- function(pos) {
      result <- gtools::permutations(3, 3, c(0,-1,1), repeats.allowed = T) %>%
        matrix(ncol = 3, byrow = F)
      result[,1] <- result[,1] + pos[[1]]
      result[,2] <- result[,2] + pos[[2]]
      result[,3] <- result[,3] + pos[[3]]
      result[which(
        between(result[,1], 1, dim[1]) & 
         between(result[,2], 1, dim[2]) &
         between(result[,3], 1, dim[3])),]
    }

    for(i in 1:6) {
      next_state <- array(0, dim = dim)
      for(x in 1:dim[1]) {
        for(y in 1:dim[2]) {
          for(z in 1:dim[3]) {
            pop <- sum(state[neighbors(c(x,y,z))]) - state[x,y,z]
            next_state[x,y,z] <- 
              (!state[x,y,z] & pop == 3) | 
              (state[x,y,z] & between(pop, 2, 3))
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

    dim <- c(n + 2 * steps, n + 2 * steps, 1 + 2 * steps, 1 + 2 * steps)
    state <- array(0, dim = dim)
    for(x in 1:n) {
      for(y in 1:n) {
        state[x+6,y+6,7,7] <- input[x,y]
      }
    }

    neighbors <- function(pos) {
      result <- gtools::permutations(3, 4, c(0,-1,1), repeats.allowed = T) %>%
        matrix(ncol = 4, byrow = F)
      result[,1] <- result[,1] + pos[[1]]
      result[,2] <- result[,2] + pos[[2]]
      result[,3] <- result[,3] + pos[[3]]
      result[,4] <- result[,4] + pos[[4]]
      result[which(
        between(result[,1], 1, dim[[1]]) & 
         between(result[,2], 1, dim[[2]]) &
         between(result[,3], 1, dim[[3]]) &
         between(result[,4], 1, dim[[4]])),]
    }

    for(i in 1:6) {
      next_state <- array(0, dim = dim)
      for(x in 1:dim[1]) {
        for(y in 1:dim[2]) {
          for(z in 1:dim[3]) {
            for(w in 1:dim[4]) {
              pop <- sum(state[neighbors(c(x,y,z,w))]) - state[x,y,z,w]
              next_state[x,y,z,w] <- 
                (!state[x,y,z,w] & pop == 3) | 
                (state[x,y,z,w] & between(pop, 2, 3))
            }
          }
        }
      }
      state <- next_state
    }
    sum(state)

    ## [1] 2280
