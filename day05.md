— Day 5: Binary Boarding —
================
Fleur Kelpin
Dec 5, 2020

    library(tidyverse)
    input <- read_lines("day05.txt")
    as_tibble(input)

    ## # A tibble: 876 x 1
    ##    value     
    ##    <chr>     
    ##  1 BFBBBBFRLL
    ##  2 FBFBFFBRLL
    ##  3 BBFBBFBLRL
    ##  4 BFFFBBFLLR
    ##  5 BFFFFBBLRL
    ##  6 BFBBFBFRRL
    ##  7 FBBFBFBRRR
    ##  8 FFBFBFFRLR
    ##  9 BBBFFBFLLL
    ## 10 BFFBBBBRRL
    ## # … with 866 more rows

# Part 1

The first 7 characters will either be F or B; these specify exactly one
of the 128 rows on the plane (numbered 0 through 127). Each letter tells
you which half of a region the given seat is in. Start with the whole
list of rows; the first letter indicates whether the seat is in the
front (0 through 63) or the back (64 through 127). The next letter
indicates which half of that region the seat is in, and so on until
you’re left with exactly one row.

The last three characters will be either L or R; these specify exactly
one of the 8 columns of seats on the plane (numbered 0 through 7). The
same process as above proceeds again, this time with only three steps. L
means to keep the lower half, while R means to keep the upper half.

Every seat also has a unique seat ID: multiply the row by 8, then add
the column. In this example, the seat has ID 44 \* 8 + 5 = 357.

As a sanity check, look through your list of boarding passes. What is
the highest seat ID on a boarding pass?

    seats <- input %>%
      str_replace_all("B", "1") %>%
      str_replace_all("F", "0") %>%
      str_replace_all("R", "1") %>%
      str_replace_all("L", "0") %>%
      strtoi(2)
    max(seats)

    ## [1] 930

# Part 2

Ding! The “fasten seat belt” signs have turned on. Time to find your
seat.

It’s a completely full flight, so your seat should be the only missing
boarding pass in your list. However, there’s a catch: some of the seats
at the very front and back of the plane don’t exist on this aircraft, so
they’ll be missing from your list as well.

Your seat wasn’t at the very front or back, though; the seats with IDs
+1 and -1 from yours will be in your list.

What is the ID of your seat?

    candidates <- (min(seats)+1):(max(seats)-1)
    candidates[!candidates %in% seats]

    ## [1] 515
