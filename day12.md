— Day 12: Rain Risk —
================
Fleur Kelpin
Dec 12, 2020

    library(tidyverse)
    input <- read_csv(file = "day12.txt", col_names = "input") %>%
      extract(input, "(\\w)(\\d+)", into = c("action", "value"), convert = TRUE)
    input

    ## # A tibble: 773 x 2
    ##    action value
    ##    <chr>  <int>
    ##  1 W          5
    ##  2 F         63
    ##  3 S          1
    ##  4 L         90
    ##  5 F         89
    ##  6 W          4
    ##  7 F         45
    ##  8 W          4
    ##  9 F         71
    ## 10 R         90
    ## # … with 763 more rows

# Part 1

The navigation instructions (your puzzle input) consists of a sequence
of single-character actions paired with integer input values. After
staring at them for a few minutes, you work out what they probably mean:

-   Action N means to move north by the given value.
-   Action S means to move south by the given value.
-   Action E means to move east by the given value.
-   Action W means to move west by the given value.
-   Action L means to turn left the given number of degrees.
-   Action R means to turn right the given number of degrees.
-   Action F means to move forward by the given value in the direction
    the ship is currently facing.

The ship starts by facing east. Only the L and R actions change the
direction the ship is facing. (That is, if the ship is facing east and
the next instruction is N10, the ship would move north 10 units, but
would still move east if the following action were F.)

Figure out where the navigation instructions lead. What is the Manhattan
distance between that location and the ship’s starting position?

    rotation_matrix <- function(degrees) {
      matrix(c(
        cospi(degrees / 180), -sinpi(degrees / 180),
        sinpi(degrees / 180), cospi(degrees / 180)
      ),
      nrow = 2
      )
    }

    location <- c(0, 0)
    direction <- c(1, 0)

    for (i in 1:nrow(input)) {
      value <- input$value[[i]]
      switch(
        input$action[[i]],
        F = location <- location + direction * value,
        N = location <- location + c(0, 1) * value,
        E = location <- location + c(1, 0) * value,
        S = location <- location + c(0, -1) * value,
        W = location <- location + c(-1, 0) * value,
        R = direction <- (rotation_matrix(value) %*% direction)[, 1],
        L = direction <- (rotation_matrix(-value) %*% direction)[, 1]
      )
    }

    sum(abs(location))

    ## [1] 1221

# Part 2

Before you can give the destination to the captain, you realize that the
actual action meanings were printed on the back of the instructions the
whole time.

Almost all of the actions indicate how to move a waypoint which is
relative to the ship’s position:

-   Action N means to move the waypoint north by the given value.
-   Action S means to move the waypoint south by the given value.
-   Action E means to move the waypoint east by the given value.
-   Action W means to move the waypoint west by the given value.
-   Action L means to rotate the waypoint around the ship left
    (counter-clockwise) the given number of degrees.
-   Action R means to rotate the waypoint around the ship right
    (clockwise) the given number of degrees.
-   Action F means to move forward to the waypoint a number of times
    equal to the given value.

The waypoint starts 10 units east and 1 unit north relative to the ship.
The waypoint is relative to the ship; that is, if the ship moves, the
waypoint moves with it.

Figure out where the navigation instructions actually lead. What is the
Manhattan distance between that location and the ship’s starting
position?

    location <- c(0, 0)
    waypoint <- c(10, 1)

    for (i in 1:nrow(input)) {
      value <- input$value[[i]]
      switch(
        input$action[[i]],
        F = location <- location + waypoint * value,
        N = waypoint <- waypoint + c(0, 1) * value,
        E = waypoint <- waypoint + c(1, 0) * value,
        S = waypoint <- waypoint + c(0, -1) * value,
        W = waypoint <- waypoint + c(-1, 0) * value,
        R = waypoint <- (rotation_matrix(value) %*% waypoint)[, 1],
        L = waypoint <- (rotation_matrix(-value) %*% waypoint)[, 1]
      )
    }

    sum(abs(location))

    ## [1] 59435
