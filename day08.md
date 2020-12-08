— Day 8: Handheld Halting —
================
Fleur Kelpin
Dec 8, 2020

    library(tidyverse)
    input <- readr::read_lines("day08.txt") %>%
      tibble(instruction=.)
    input

    ## # A tibble: 617 x 1
    ##    instruction
    ##    <chr>      
    ##  1 acc -5     
    ##  2 nop +333   
    ##  3 acc +45    
    ##  4 jmp +288   
    ##  5 acc -9     
    ##  6 jmp +1     
    ##  7 acc +27    
    ##  8 jmp +464   
    ##  9 acc +34    
    ## 10 jmp +478   
    ## # … with 607 more rows

# Part 1

First let’s parse the instructions.

    program <- input %>%
      extract(instruction,
              into=c("operation", "argument"),
              regex="(\\w+) ([+-]\\d+)",
              convert = TRUE)
    program

    ## # A tibble: 617 x 2
    ##    operation argument
    ##    <chr>        <int>
    ##  1 acc             -5
    ##  2 nop            333
    ##  3 acc             45
    ##  4 jmp            288
    ##  5 acc             -9
    ##  6 jmp              1
    ##  7 acc             27
    ##  8 jmp            464
    ##  9 acc             34
    ## 10 jmp            478
    ## # … with 607 more rows

Then implement the instructions

    state <- list(acc=0, ip=0)
    nop <- function(acc, ip) {
      list(acc = acc, ip = ip + 1)
    }
    acc <- function(arg, acc, ip) {
      list(acc = acc + arg, ip = ip + 1)
    }
    jmp <- function(arg, acc, ip) {
      list(acc = acc, ip = ip + arg)
    }

    step <- function(program, s) {
      op <- program$operation[[s$ip + 1]]
      arg <- program$argument[[s$ip + 1]]
      switch(op,
             "nop" = nop(s$acc, s$ip),
             "acc" = acc(arg, s$acc, s$ip),
             "jmp" = jmp(arg, s$acc, s$ip)
             )
    }
    step(program, state)

    ## $acc
    ## [1] -5
    ## 
    ## $ip
    ## [1] 1

Then we’re ready to solve part 1:

    findLoop <- function(program) {
      state <- list(acc=0, ip=0)
      visited <- c()
      while(!state$ip %in% visited) {
        visited <- append(visited, state$ip)
        if (state$ip > nrow(program)-1) {
          print("Terminated!")
          print(state)
          return(state)
        }
        state <- step(program, state)
      }
      state
    }
    findLoop(program)

    ## $acc
    ## [1] 1709
    ## 
    ## $ip
    ## [1] 439

# Part 2

    fix <- function (program, i) {
      op <- program$operation[[i]]
      fixed <- program
      if (op == 'jmp'){
        fixed$operation[[i]] = 'nop'
      }
      if (op == 'nop'){
        fixed$operation[[i]] = 'jmp'
      }
      fixed
    }

    for(i in 1:nrow(program)) {
      program %>%
        fix(i) %>%
        findLoop
    }

    ## [1] "Terminated!"
    ## $acc
    ## [1] 1976
    ## 
    ## $ip
    ## [1] 617
