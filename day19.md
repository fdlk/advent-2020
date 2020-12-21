— Day 19: Monster Messages —
================
Fleur Kelpin
Dec 19, 2020

    library(tidyverse)
    input <- tibble(line = readLines("day19-rules.txt")) %>%
      extract(line, c("number", "rules"), "(\\d+): (.*)") %>%
      extract(rules, "letters", "\"(\\w)\"", remove = F) %>%
      extract(rules, c("alt1", NA, "alt2"), "([0-9 ]*)(\\|(.*))?") %>%
      mutate(number = as.integer(number)) %>%
      extract(alt1, c("alt1a", "alt1b"), "([0-9]+)( [0-9]+)?", convert = T) %>%
      extract(alt2, c("alt2a", "alt2b"), "([0-9]+)( [0-9]+)?", convert = T) %>%
      arrange(number)
    input

    ## # A tibble: 131 x 6
    ##    number alt1a alt1b alt2a alt2b letters
    ##     <int> <int> <int> <int> <int> <chr>  
    ##  1      0     8    11    NA    NA <NA>   
    ##  2      1    39    64   110    44 <NA>   
    ##  3      2   124   110    88    39 <NA>   
    ##  4      3    39     5   110   100 <NA>   
    ##  5      4    39    74   110    44 <NA>   
    ##  6      5   109   110   117    39 <NA>   
    ##  7      6    12    39   128   110 <NA>   
    ##  8      7   101    39    13   110 <NA>   
    ##  9      8    42    NA    NA    NA <NA>   
    ## 10      9    24   110    89    39 <NA>   
    ## # … with 121 more rows

    messages <- tibble(message = readLines("day19-messages.txt"))
    messages

    ## # A tibble: 502 x 1
    ##    message                                                                      
    ##    <chr>                                                                        
    ##  1 aaababaaaaaabbbaaaabbaaa                                                     
    ##  2 aaaabbbaaaaaabaaabbabaabbbbaabaaabbbababbbbabbabababaaaa                     
    ##  3 aabaababaabaaaaabbbbbbbbaaabbbaabababbabbaababaabbbabbabaababaaababaaabaaaab…
    ##  4 ababbabaabaaaabbbbbbaaabaabbbaabbaaaabbababbaaaaababbbbb                     
    ##  5 aabbbababbabbbbaaaabbaba                                                     
    ##  6 baababbaaabbbabbbbbaabbbbaabbaaaaabbbbab                                     
    ##  7 aaaabbbaaabbaabbbaabbabb                                                     
    ##  8 baaaabbbabaaabbbbbabbaabbbaabbabbbbaababaabbbbabbababababbaaabbb             
    ##  9 aabbbababbbaaabbbabbbbba                                                     
    ## 10 abbabbaabaabbbabbbaabaaaabbbbbaabbaaaabb                                     
    ## # … with 492 more rows

# Part 1

    getMessages <- function(num) {
      if (is.na(num)) {
        return(character())
      }
      rule <- input %>% filter(number == num)
      letters <- rule$letters[[1]]
      if (!is.na(letters)) {
        return(letters)
      }
      alt1a <- rule$alt1a[[1]]
      alt1b <- rule$alt1b[[1]]
      alt2a <- rule$alt2a[[1]]
      alt2b <- rule$alt2b[[1]]
      if (!is.na(alt2a)) {
        union(
          getMessagesAB(alt1a, alt1b),
          getMessagesAB(alt2a, alt2b)
        ) %>%
          unique()
      } else {
        getMessagesAB(alt1a, alt1b)
      }
    }
    getMessagesAB <- function(pre, post) {
      a <- getMessages(pre)
      if (is.na(post)) {
        return(a)
      }
      b <- getMessages(post)
      crossing(a, b) %>%
        mutate(x = paste0(a, b)) %>%
        {
          .$x
        } %>%
        unique()
    }

    messages$message %>%
      intersect(getMessages(0)) %>%
      length()

    ## [1] 299

# Part 2

As you look over the list of messages, you realize your matching rules
aren’t quite right. To fix them, completely replace rules 8: 42 and 11:
42 31 with the following:

    8: 42 | 42 8
    11: 42 31 | 42 11 31

The other relevant rule is `0: 8 11`

    getRegex <- function(num) {
      getMessages(num) %>%
        paste(collapse = "|") %>%
        {paste0("(", ., ")")}
    }
    reg42 <- getRegex(42)
    reg11 <- getRegex(11)
    reg31 <- getRegex(31)
    regex <- paste0("^", reg42, "+((", reg42, reg31, ")|(", reg42, "{2}", reg31,
                    "{2})|(", reg42, "{3}", reg31, "{3})|(", reg42, "{4}", reg31,
                    "{4}))$")
    messages %>%
      rowwise() %>%
      filter(str_detect(message, regex)) %>%
      nrow()

    ## [1] 414
