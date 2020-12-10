— Day 10: Adapter Array —
================
Fleur Kelpin
Dec 10, 2020

    library(tidyverse)
    input <- readr::read_csv("day10.txt", col_names = FALSE)[[1]] %>%
      prepend(0) %>%
      {append(., max(.) + 3)} %>%
      sort()
      
    as_tibble(input)

    ## # A tibble: 104 x 1
    ##    value
    ##    <dbl>
    ##  1     0
    ##  2     1
    ##  3     2
    ##  4     3
    ##  5     4
    ##  6     7
    ##  7    10
    ##  8    11
    ##  9    12
    ## 10    15
    ## # … with 94 more rows

# Part 1

Find a chain that uses all of your adapters to connect the charging
outlet to your device’s built-in adapter and count the joltage
differences between the charging outlet, the adapters, and your device.
**What is the number of 1-jolt differences multiplied by the number of
3-jolt differences?**

    input %>%
      embed(2) %>%
      { .[, 1] - .[, 2] } %>%
      { sum(. == 1) * sum(. == 3) }

    ## [1] 2346

# Part 2

**What is the total number of distinct ways you can arrange the adapters
to connect the charging outlet to your device?**

Use dynamic programming to reuse the number of ways you can connect the
lower jolt adapters to the outlet to compute the number of ways for the
next adapter.

    adapters <- tibble(jolt = input, ways=as.numeric(NA))
    adapters[1, 'ways'] = 1

    while((index <- detect_index(adapters$ways, is.na)) > 0 ){
      joltage <- adapters[index, 'jolt']
      prev_adapters <- filter(adapters, jolt %>% between(joltage-3, joltage-1))
      adapters[index, 'ways'] = sum(prev_adapters$ways)
    }
    adapters

    ## # A tibble: 104 x 2
    ##     jolt  ways
    ##    <dbl> <dbl>
    ##  1     0     1
    ##  2     1     1
    ##  3     2     2
    ##  4     3     4
    ##  5     4     7
    ##  6     7     7
    ##  7    10     7
    ##  8    11     7
    ##  9    12    14
    ## 10    15    14
    ## # … with 94 more rows

    options(digits = 15)
    max(adapters$ways)

    ## [1] 6044831973376
