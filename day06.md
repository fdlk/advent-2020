— Day 6: Custom Customs —
================
Fleur Kelpin
Dec 6, 2020

    library(tidyverse)
    input <- readr::read_file("day06.txt") %>%
      str_split("\n\n") %>%
      unlist()
    tibble(input)

    ## # A tibble: 460 x 1
    ##    input                                                                     
    ##    <chr>                                                                     
    ##  1 "nefaym\neynamf\neafnmy\nafnmey"                                          
    ##  2 "zmd\nalo\nekvrtpyuqi\nhwmc\nszgah"                                       
    ##  3 "y\nmzqys\nycsq\nyrtxvnkdglj"                                             
    ##  4 "egubpzyrhlojkixfn\nhyifblockgenrzjxp\nzxgbkohlpyfjinre\nrzolbfihpkxnygje"
    ##  5 "wvjoehlkxnyz\nevwkobnhy\nnerkyowgv\ntaypknsdovmuwice\nqjvknewory"        
    ##  6 "ncypkuxsizla\nndjoiqhetrgmwfvb"                                          
    ##  7 "tfhnk\nkmnftzh\nhkntf\nnftkh"                                            
    ##  8 "wj\nw\nw\nwq\nw"                                                         
    ##  9 "jzaw\njzw"                                                               
    ## 10 "gysdxblojkvnmr\nkhymldnxgtbvirj\nbydgmxwjvrkfln\nlykvumdnrxbjg"          
    ## # … with 450 more rows

# Part 1

For each group, count the number of questions to which anyone answered
“yes”. What is the sum of those counts?

    input %>%
      str_match_all("\\w") %>%
      map_dbl(
        ~ unique(.) %>%
          length()
      ) %>%
      sum()

    ## [1] 6259

# Part 2

For each group, count the number of questions to which everyone answered
“yes”. What is the sum of those counts?

    input %>%
      str_split("\n") %>%
      map_dbl(
        ~ str_split(., "") %>%
          reduce(intersect) %>%
          length()
      ) %>%
      sum()

    ## [1] 3178
