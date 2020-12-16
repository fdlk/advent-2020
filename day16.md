— Day 16: Ticket Translation —
================
Fleur Kelpin
Dec 16, 2020

    library(tidyverse)
    fields <- tibble(line = readLines("day16-fields.txt")) %>%
      extract(line,
        into = c("field", "from1", "to1", "from2", "to2"),
        "([^:]*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)",
        convert = T
      )
    fields

    ## # A tibble: 20 x 5
    ##    field              from1   to1 from2   to2
    ##    <chr>              <int> <int> <int> <int>
    ##  1 departure location    33   430   456   967
    ##  2 departure station     42   864   875   957
    ##  3 departure platform    42   805   821   968
    ##  4 departure track       34    74    93   967
    ##  5 departure date        40   399   417   955
    ##  6 departure time        30   774   797   950
    ##  7 arrival location      50   487   507   954
    ##  8 arrival station       34   693   718   956
    ##  9 arrival platform      42   729   751   959
    ## 10 arrival track         28   340   349   968
    ## 11 class                 49   524   543   951
    ## 12 duration              40   372   397   951
    ## 13 price                 48   922   939   951
    ## 14 route                 33   642   666   960
    ## 15 row                   39   238   255   973
    ## 16 seat                  48   148   161   973
    ## 17 train                 50   604   630   971
    ## 18 type                  29   299   316   952
    ## 19 wagon                 45   898   921   966
    ## 20 zone                  34   188   212   959

    your_ticket <-
      "137,173,167,139,73,67,61,179,103,113,163,71,97,101,109,59,131,127,107,53"
    nearby_tickets <- tibble(values = readLines("day16-nearby.txt"))
    nearby_tickets

    ## # A tibble: 245 x 1
    ##    values                                                                       
    ##    <chr>                                                                        
    ##  1 122,945,480,667,824,475,800,224,297,602,673,513,641,524,835,981,54,184,60,721
    ##  2 692,125,595,331,803,765,721,249,729,162,226,523,821,137,297,588,296,299,720,…
    ##  3 851,112,324,163,805,544,563,372,590,228,264,590,290,491,799,677,881,573,584,…
    ##  4 318,471,224,717,144,113,521,320,280,234,522,833,515,514,543,630,128,221,279,…
    ##  5 466,234,233,845,296,757,675,841,113,135,464,732,220,575,769,693,231,187,548,…
    ##  6 179,471,485,215,287,334,848,153,546,473,238,675,487,56,595,484,279,366,692,8…
    ##  7 532,422,215,370,552,577,171,757,58,686,169,830,849,52,422,128,218,630,424,419
    ##  8 113,466,896,241,98,552,339,561,55,564,279,757,514,825,124,842,464,668,222,358
    ##  9 547,888,333,370,221,20,834,100,555,339,721,860,557,477,430,635,230,467,290,8…
    ## 10 599,166,826,588,831,129,188,669,74,820,640,876,669,825,231,163,280,282,64,474
    ## # … with 235 more rows

# Part 1

Start by determining which tickets are completely invalid; these are
tickets that contain values which aren’t valid for any field. Ignore
your ticket for now.

Adding together all of the invalid values produces your ticket scanning
error rate.

Consider the validity of the nearby tickets you scanned. What is your
ticket scanning error rate?

    valid_value <- function(value) {
      fields %>%
        rowwise() %>%
        filter(between(value, from1, to1) | between(value, from2, to2)) %>%
        nrow() > 0
    }

    ticket_scanning_error_rate <- function(values) {
      invalid <- tibble(value = values) %>%
        separate_rows(value, sep = ",", convert = T) %>%
        rowwise() %>%
        filter(!valid_value(value))
      if (nrow(invalid) == 0) NA else sum(invalid$value)
    }

    part1 <- nearby_tickets %>%
      rowwise() %>%
      mutate(error = ticket_scanning_error_rate(values))
    sum(part1$error, na.rm = TRUE)

    ## [1] 26026

# Part 2

Now that you’ve identified which tickets contain invalid values, discard
those tickets entirely. Use the remaining valid tickets to determine
which field is which.

    valid_tickets <- part1 %>%
      filter(is.na(error)) %>%
      {
        c(.$values, your_ticket)
      }

Using the valid ranges for each field, determine what order the fields
appear on the tickets. The order is consistent between all tickets: if
seat is the third field, it is the third field on every ticket,
including your ticket.

    valid_values <- valid_tickets %>%
      map(function(value) {
        str_split(value, ",")
      }) %>%
      unlist() %>%
      as.integer() %>%
      matrix(nrow = length(valid_tickets), byrow = T)

We’re drowning in numbers here, let’s try and summarize them in a usable
way.

    # index_options[[i]] is true if this field can be column with index i in the
    # valid_values list
    index_options <- function(from1, to1, from2, to2) {
      valid_values %>%
        map_lgl(~ between(., from1, to1) | between(., from2, to2)) %>%
        matrix(nrow = length(valid_tickets), byrow = F) %>%
        colSums() %>%
        map_lgl(~ . == length(valid_tickets))
    }

Matrix `x[i,j]` indicates if field `i` can have index `j` on the ticket.

    x <- fields %>%
      rowwise() %>%
      mutate(index_options = list(index_options(from1, to1, from2, to2)))
    x <- matrix(unlist(x$index_options), ncol = nrow(fields), byrow = T)

Now if a row has only one TRUE value, we’ve found its index on the
ticket. Fill in the index, and set the row and column to false, cause we
know that other fields must match other indices. Rinse and repeat.

    fields$index <- NA_integer_
    while (any(x)) {
      row <- rowSums(x) %>%
        map_lgl(~ . == 1) %>%
        which()
      row <- row[[1]]
      col <- which(x[row, ])
      col <- col[[1]]
      fields$index[row] <- col
      x[row, ] <- F
      x[, col] <- F
    }

Once you work out which field is which, look for the six fields on your
ticket that start with the word departure. What do you get if you
multiply those six values together?

    options(digits = 22)
    fields %>%
      filter(str_detect(field, "departure")) %>%
      rowwise() %>%
      mutate(value = as.integer(unlist(str_split(your_ticket, ","))[[index]])) %>%
      ungroup() %>%
      summarize(prod(value))

    ## # A tibble: 1 x 1
    ##   `prod(value)`
    ##           <dbl>
    ## 1 1305243193339

    fields %>%
      arrange(index)

    ## # A tibble: 20 x 6
    ##    field              from1   to1 from2   to2 index
    ##    <chr>              <int> <int> <int> <int> <int>
    ##  1 zone                  34   188   212   959     1
    ##  2 departure location    33   430   456   967     2
    ##  3 arrival track         28   340   349   968     3
    ##  4 departure station     42   864   875   957     4
    ##  5 price                 48   922   939   951     5
    ##  6 arrival location      50   487   507   954     6
    ##  7 departure track       34    74    93   967     7
    ##  8 route                 33   642   666   960     8
    ##  9 departure date        40   399   417   955     9
    ## 10 train                 50   604   630   971    10
    ## 11 departure platform    42   805   821   968    11
    ## 12 class                 49   524   543   951    12
    ## 13 arrival platform      42   729   751   959    13
    ## 14 row                   39   238   255   973    14
    ## 15 seat                  48   148   161   973    15
    ## 16 wagon                 45   898   921   966    16
    ## 17 arrival station       34   693   718   956    17
    ## 18 type                  29   299   316   952    18
    ## 19 duration              40   372   397   951    19
    ## 20 departure time        30   774   797   950    20
