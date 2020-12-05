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
        into = c("min", "max", "char", "pwd"),
        convert = TRUE
      )
    input

    ## # A tibble: 1,000 x 4
    ##      min   max char  pwd                
    ##    <int> <int> <chr> <chr>              
    ##  1     1     4 n     nnnnn              
    ##  2     5     7 z     qhcgzzz            
    ##  3     7    11 m     mmmmmmsmmmmm       
    ##  4     5     8 d     ldddtdgnzddddwl    
    ##  5    16    18 q     qsqqqqqqqqqpqqqlqhq
    ##  6     5     7 s     bwkbdlwns          
    ##  7    14    17 v     vvvvvvvvvvvvvpvvxv 
    ##  8     4     5 v     mvkvvn             
    ##  9     2     5 h     lcwghhkpkxvzkvrmxrv
    ## 10     2     9 m     kmdvdlvxmhgsmlzp   
    ## # … with 990 more rows

# Part 1

Each line gives the password policy and then the password. The password
policy indicates the lowest and highest number of times a given letter
must appear for the password to be valid. For example, 1-3 a means that
the password must contain a at least 1 time and at most 3 times.

**How many passwords are valid** according to their policies?

    input %>%
      mutate(occurs = str_count(pwd, char)) %>%
      filter(occurs %>% between(min, max)) %>%
      count()

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   411

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
