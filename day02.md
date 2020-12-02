— Day 2: Password Philosophy —
================
Fleur Kelpin
Dec 2, 2020

    library(tidyverse)
    input <- read_csv("day02.txt",
      col_names = "line",
      col_types = cols(line = col_character())
    ) %>%
      extract(line,
        "(\\d+)-(\\d+) (\\w): (\\w+)",
        into = c("min", "max", "char", "pwd")
      ) %>%
      mutate(min = strtoi(min, 10), max = strtoi(max, 10))

# Part 1

Each line gives the password policy and then the password. The password
policy indicates the lowest and highest number of times a given letter
must appear for the password to be valid. For example, 1-3 a means that
the password must contain a at least 1 time and at most 3 times.

**How many passwords are valid** according to their policies?

    input %>%
      mutate(occurs = str_count(pwd, char)) %>%
      filter(occurs >= min) %>%
      filter(occurs <= max) %>%
      count()

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   456

# Part 2

Each policy actually describes two positions in the password, where 1
means the first character, 2 means the second character, and so on. (Be
careful; Toboggan Corporate Policies have no concept of “index zero”!)
**Exactly one of these positions** must contain the given letter. Other
occurrences of the letter are irrelevant for the purposes of policy
enforcement.

    input %>%
      mutate(first = str_sub(pwd, min, min)) %>%
      mutate(second = str_sub(pwd, max, max)) %>%
      filter(char == first | char == second) %>%
      filter(first != second) %>%
      count()

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   308