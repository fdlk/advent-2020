— Day 7: Handy Haversacks —
================
Fleur Kelpin
Dec 7, 2020

    library(tidyverse)
    input <- readr::read_lines("day07.txt") %>%
      as_tibble()
    input

    ## # A tibble: 594 x 1
    ##    value                                                                        
    ##    <chr>                                                                        
    ##  1 dark indigo bags contain 2 clear indigo bags.                                
    ##  2 mirrored gold bags contain 2 pale blue bags, 1 dark violet bag.              
    ##  3 dim aqua bags contain 1 mirrored beige bag.                                  
    ##  4 dim fuchsia bags contain 5 dark red bags, 1 muted green bag, 4 clear indigo …
    ##  5 shiny fuchsia bags contain 4 wavy teal bags, 4 dim cyan bags, 4 light green …
    ##  6 posh chartreuse bags contain 2 faded blue bags, 2 pale plum bags, 2 posh cor…
    ##  7 striped red bags contain 3 vibrant red bags.                                 
    ##  8 striped purple bags contain 5 shiny brown bags.                              
    ##  9 dark silver bags contain 1 dull aqua bag, 4 dim magenta bags.                
    ## 10 dark chartreuse bags contain 2 plaid black bags, 4 light bronze bags, 5 dott…
    ## # … with 584 more rows

# Part 1

How many bag colors can eventually contain at least one shiny gold bag?

    who_contains <- function(type) {
      containers <- str_match(
        input$value,
        paste0(
          "(.+) bags contain .*(\\d)+ ",
          type,
          " bags?"
        )
      )[, 2]
      containers[!is.na(containers)]
    }
    who_contains("shiny gold")

    ## [1] "plaid gold"    "bright violet" "clear brown"   "clear blue"   
    ## [5] "faded gold"    "drab purple"

    all_contains <- function(containers, checked) {
      new_containers <- containers %>%
        map(who_contains) %>%
        unlist() %>%
        unique() %>%
        setdiff(checked)
      num_found <- length(new_containers)
      if (num_found == 0) {
        return(0)
      }
      length(new_containers) + all_contains(
        new_containers,
        unique(c(
          checked,
          containers,
          new_containers
        ))
      )
    }
    all_contains("shiny gold", c())

    ## [1] 233

# Part 2

How many individual bags are required inside your single shiny gold bag?

    contents <- function(type) {
      inside <- input %>%
        extract(value, paste0(type, " bags contain (.*)"), into = "line") %>%
        separate_rows(line, sep = ",") %>%
        extract(line,
          "(\\d+) (.*) bags?",
          convert = TRUE, into = c("num", "type")
        ) %>%
        drop_na()
      if (nrow(inside) == 0) {
        return(0)
      }
      inside %>%
        rowwise() %>%
        mutate(total = num + num * contents(type)) %>%
        {
          sum(.$total)
        }
    }

    contents("shiny gold")

    ## [1] 421550
