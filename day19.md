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

    getRegex <- function(number) {
      if (is.na(number)) {
        return("")
      }
      if (!is.na(input$letters[[number + 1]])) {
        return(input$letters[[number + 1]])
      }
      if (!is.na(input$alt2a[[number + 1]])) {
        paste0(
          "(",
          getRegex(input$alt1a[[number + 1]]),
          getRegex(input$alt1b[[number + 1]]),
          "|",
          getRegex(input$alt2a[[number + 1]]),
          getRegex(input$alt2b[[number + 1]]),
          ")"
        )
      } else {
        paste0(
          "(",
          getRegex(input$alt1a[[number + 1]]),
          getRegex(input$alt1b[[number + 1]]),
          ")"
        )
      }
    }
    getRegex(0)

    ## [1] "(((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b))((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b)((b((((b(bb)|a(aa))a|(a(bb))b)b|(((ab)a|(ba|aa)b)b|(a(ab)|b(ab|aa))a)a)b|((b(a(ab|bb)|b(aa))|a(b(ba|aa)|a(bb|aa)))a|((((a|b)(a|b))a|(bb|aa)b)a|((ab|b(a|b))b|(ab|aa)a)b)b)a)|a(a((b(a((a|b)a|ab)|b(bb))|a(a((a|b)b|aa)|b((a|b)(a|b))))b|(a((ab|aa)a|(aa|b(a|b))b)|b(b(bb|aa)|a(bb)))a)|b((a((ab|b(a|b))b|(bb|ba)a)|b((ab|bb)b|(bb|ba)a))b|(a(b((a|b)(a|b))|a(ab))|b(a(aa)|b(ab|aa)))a)))b|(a(b(a(b(b(bb)|a((a|b)(a|b)))|a((ba|ab)b|(bb)a))|b(((a|b)(ba|aa))b|((ab|aa)a|(aa|b(a|b))b)a))|a(((b(ab|bb)|a(bb|ba))a|((ba)b)b)a|(a((a|b)(ba|aa))|b((ba|aa)a|((a|b)a|ab)b))b))|b(a(b(a(((a|b)(a|b))a|(bb|aa)b)|b(a(ba)|b(bb)))|a(a((aa|b(a|b))b|((a|b)b|aa)a)|b(((a|b)(a|b))a|(bb|aa)b)))|b(a(a((aa|b(a|b))b|((a|b)b|aa)a)|b((aa)b))|b(b(a(ab)|b(ab|aa))|a((ba)a|((a|b)a|ab)b)))))a)))"

    messages %>%
      rowwise() %>%
      filter(str_detect(message, paste0("^", getRegex(0), "$"))) %>%
      nrow()

    ## [1] 299

# Part 2

    getRegex2 <- function(number) {
      if (is.na(number)) {
        return("")
      }
      if (number == 8) {
        return(paste0("(", getRegex2(42), ")+"))
      }
      if (number == 11) {
        return(
          paste0(
            "(",
            getRegex2(42),
            getRegex2(31),
            ")|(",
            getRegex2(42),
            "{2}",
            getRegex2(31),
            "{2}",
            ")|(",
            getRegex2(42),
            "{3}",
            getRegex2(31),
            "{3}",
            ")|(",
            getRegex2(42),
            "{4}",
            getRegex2(31),
            "{4}",
            ")|(",
            getRegex2(42),
            "{5}",
            getRegex2(31),
            "{5}",
            ")"
          )
        )
      }
      if (!is.na(input$letters[[number + 1]])) {
        return(input$letters[[number + 1]])
      }
      if (!is.na(input$alt2a[[number + 1]])) {
        paste0(
          "(",
          getRegex2(input$alt1a[[number + 1]]),
          getRegex2(input$alt1b[[number + 1]]),
          "|",
          getRegex2(input$alt2a[[number + 1]]),
          getRegex2(input$alt2b[[number + 1]]),
          ")"
        )
      } else {
        paste0(
          "(",
          getRegex2(input$alt1a[[number + 1]]),
          getRegex2(input$alt1b[[number + 1]]),
          ")"
        )
      }
    }
    getRegex2(0)

    ## [1] "(((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b))+((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b)((b((((b(bb)|a(aa))a|(a(bb))b)b|(((ab)a|(ba|aa)b)b|(a(ab)|b(ab|aa))a)a)b|((b(a(ab|bb)|b(aa))|a(b(ba|aa)|a(bb|aa)))a|((((a|b)(a|b))a|(bb|aa)b)a|((ab|b(a|b))b|(ab|aa)a)b)b)a)|a(a((b(a((a|b)a|ab)|b(bb))|a(a((a|b)b|aa)|b((a|b)(a|b))))b|(a((ab|aa)a|(aa|b(a|b))b)|b(b(bb|aa)|a(bb)))a)|b((a((ab|b(a|b))b|(bb|ba)a)|b((ab|bb)b|(bb|ba)a))b|(a(b((a|b)(a|b))|a(ab))|b(a(aa)|b(ab|aa)))a)))b|(a(b(a(b(b(bb)|a((a|b)(a|b)))|a((ba|ab)b|(bb)a))|b(((a|b)(ba|aa))b|((ab|aa)a|(aa|b(a|b))b)a))|a(((b(ab|bb)|a(bb|ba))a|((ba)b)b)a|(a((a|b)(ba|aa))|b((ba|aa)a|((a|b)a|ab)b))b))|b(a(b(a(((a|b)(a|b))a|(bb|aa)b)|b(a(ba)|b(bb)))|a(a((aa|b(a|b))b|((a|b)b|aa)a)|b(((a|b)(a|b))a|(bb|aa)b)))|b(a(a((aa|b(a|b))b|((a|b)b|aa)a)|b((aa)b))|b(b(a(ab)|b(ab|aa))|a((ba)a|((a|b)a|ab)b)))))a))|((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b){2}((b((((b(bb)|a(aa))a|(a(bb))b)b|(((ab)a|(ba|aa)b)b|(a(ab)|b(ab|aa))a)a)b|((b(a(ab|bb)|b(aa))|a(b(ba|aa)|a(bb|aa)))a|((((a|b)(a|b))a|(bb|aa)b)a|((ab|b(a|b))b|(ab|aa)a)b)b)a)|a(a((b(a((a|b)a|ab)|b(bb))|a(a((a|b)b|aa)|b((a|b)(a|b))))b|(a((ab|aa)a|(aa|b(a|b))b)|b(b(bb|aa)|a(bb)))a)|b((a((ab|b(a|b))b|(bb|ba)a)|b((ab|bb)b|(bb|ba)a))b|(a(b((a|b)(a|b))|a(ab))|b(a(aa)|b(ab|aa)))a)))b|(a(b(a(b(b(bb)|a((a|b)(a|b)))|a((ba|ab)b|(bb)a))|b(((a|b)(ba|aa))b|((ab|aa)a|(aa|b(a|b))b)a))|a(((b(ab|bb)|a(bb|ba))a|((ba)b)b)a|(a((a|b)(ba|aa))|b((ba|aa)a|((a|b)a|ab)b))b))|b(a(b(a(((a|b)(a|b))a|(bb|aa)b)|b(a(ba)|b(bb)))|a(a((aa|b(a|b))b|((a|b)b|aa)a)|b(((a|b)(a|b))a|(bb|aa)b)))|b(a(a((aa|b(a|b))b|((a|b)b|aa)a)|b((aa)b))|b(b(a(ab)|b(ab|aa))|a((ba)a|((a|b)a|ab)b)))))a){2})|((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b){3}((b((((b(bb)|a(aa))a|(a(bb))b)b|(((ab)a|(ba|aa)b)b|(a(ab)|b(ab|aa))a)a)b|((b(a(ab|bb)|b(aa))|a(b(ba|aa)|a(bb|aa)))a|((((a|b)(a|b))a|(bb|aa)b)a|((ab|b(a|b))b|(ab|aa)a)b)b)a)|a(a((b(a((a|b)a|ab)|b(bb))|a(a((a|b)b|aa)|b((a|b)(a|b))))b|(a((ab|aa)a|(aa|b(a|b))b)|b(b(bb|aa)|a(bb)))a)|b((a((ab|b(a|b))b|(bb|ba)a)|b((ab|bb)b|(bb|ba)a))b|(a(b((a|b)(a|b))|a(ab))|b(a(aa)|b(ab|aa)))a)))b|(a(b(a(b(b(bb)|a((a|b)(a|b)))|a((ba|ab)b|(bb)a))|b(((a|b)(ba|aa))b|((ab|aa)a|(aa|b(a|b))b)a))|a(((b(ab|bb)|a(bb|ba))a|((ba)b)b)a|(a((a|b)(ba|aa))|b((ba|aa)a|((a|b)a|ab)b))b))|b(a(b(a(((a|b)(a|b))a|(bb|aa)b)|b(a(ba)|b(bb)))|a(a((aa|b(a|b))b|((a|b)b|aa)a)|b(((a|b)(a|b))a|(bb|aa)b)))|b(a(a((aa|b(a|b))b|((a|b)b|aa)a)|b((aa)b))|b(b(a(ab)|b(ab|aa))|a((ba)a|((a|b)a|ab)b)))))a){3})|((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b){4}((b((((b(bb)|a(aa))a|(a(bb))b)b|(((ab)a|(ba|aa)b)b|(a(ab)|b(ab|aa))a)a)b|((b(a(ab|bb)|b(aa))|a(b(ba|aa)|a(bb|aa)))a|((((a|b)(a|b))a|(bb|aa)b)a|((ab|b(a|b))b|(ab|aa)a)b)b)a)|a(a((b(a((a|b)a|ab)|b(bb))|a(a((a|b)b|aa)|b((a|b)(a|b))))b|(a((ab|aa)a|(aa|b(a|b))b)|b(b(bb|aa)|a(bb)))a)|b((a((ab|b(a|b))b|(bb|ba)a)|b((ab|bb)b|(bb|ba)a))b|(a(b((a|b)(a|b))|a(ab))|b(a(aa)|b(ab|aa)))a)))b|(a(b(a(b(b(bb)|a((a|b)(a|b)))|a((ba|ab)b|(bb)a))|b(((a|b)(ba|aa))b|((ab|aa)a|(aa|b(a|b))b)a))|a(((b(ab|bb)|a(bb|ba))a|((ba)b)b)a|(a((a|b)(ba|aa))|b((ba|aa)a|((a|b)a|ab)b))b))|b(a(b(a(((a|b)(a|b))a|(bb|aa)b)|b(a(ba)|b(bb)))|a(a((aa|b(a|b))b|((a|b)b|aa)a)|b(((a|b)(a|b))a|(bb|aa)b)))|b(a(a((aa|b(a|b))b|((a|b)b|aa)a)|b((aa)b))|b(b(a(ab)|b(ab|aa))|a((ba)a|((a|b)a|ab)b)))))a){4})|((((b(a(((bb|aa)b|(bb)a)a|(b(bb))b)|b(((ab|b(a|b))(a|b))b|(a(ab|b(a|b))|b(ba|aa))a))|a(a(a(((a|b)(a|b))b|(ab|aa)a)|b(b((a|b)b|aa)|a(ab|aa)))|b(a(((a|b)b|aa)a|(bb|aa)b)|b(b(ba|aa)|a(bb)))))a|(a((b(b((a|b)(a|b))|a(ab))|a(b(bb)|a((a|b)(a|b))))b|(((ba)a)a|(b(ab)|a(bb|ba))b)a)|b(a(((a|b)(ba|aa))b|((bb)a|(ab|b(a|b))b)a)|b((((a|b)b|aa)a|(ab|b(a|b))b)b|(a(bb))a)))b)a|((a(b((a(ab|aa)|b(ab|bb))a|(b((a|b)b|aa)|a(ab))b)|a(a(b(ba|aa)|a(bb))|b((ba|aa)b|((a|b)b|aa)a)))|b(((b((a|b)b|aa)|a(ab))a|((ab|bb)b|((a|b)b|aa)a)b)a|(a((ab)b|(bb)a)|b((ab)a|(ba|aa)b))b))a|(a(((b(ba|aa)|a(bb|aa))b|(b(ba|aa)|a(bb))a)a|(a((bb)a|(ba)b)|b(a(aa)|b(ba|aa)))b)|b((b(((a|b)a|ab)a|((a|b)(a|b))b)|a(b((a|b)a|ab)|a(ab|b(a|b))))b|(((ab|bb)b|((a|b)b|aa)a)a|(a(bb|aa)|b(aa|b(a|b)))b)a))b)b){5}((b((((b(bb)|a(aa))a|(a(bb))b)b|(((ab)a|(ba|aa)b)b|(a(ab)|b(ab|aa))a)a)b|((b(a(ab|bb)|b(aa))|a(b(ba|aa)|a(bb|aa)))a|((((a|b)(a|b))a|(bb|aa)b)a|((ab|b(a|b))b|(ab|aa)a)b)b)a)|a(a((b(a((a|b)a|ab)|b(bb))|a(a((a|b)b|aa)|b((a|b)(a|b))))b|(a((ab|aa)a|(aa|b(a|b))b)|b(b(bb|aa)|a(bb)))a)|b((a((ab|b(a|b))b|(bb|ba)a)|b((ab|bb)b|(bb|ba)a))b|(a(b((a|b)(a|b))|a(ab))|b(a(aa)|b(ab|aa)))a)))b|(a(b(a(b(b(bb)|a((a|b)(a|b)))|a((ba|ab)b|(bb)a))|b(((a|b)(ba|aa))b|((ab|aa)a|(aa|b(a|b))b)a))|a(((b(ab|bb)|a(bb|ba))a|((ba)b)b)a|(a((a|b)(ba|aa))|b((ba|aa)a|((a|b)a|ab)b))b))|b(a(b(a(((a|b)(a|b))a|(bb|aa)b)|b(a(ba)|b(bb)))|a(a((aa|b(a|b))b|((a|b)b|aa)a)|b(((a|b)(a|b))a|(bb|aa)b)))|b(a(a((aa|b(a|b))b|((a|b)b|aa)a)|b((aa)b))|b(b(a(ab)|b(ab|aa))|a((ba)a|((a|b)a|ab)b)))))a){5}))"

    messages %>%
      rowwise() %>%
      filter(str_detect(message, paste0("^", getRegex2(0), "$"))) %>%
      nrow()

    ## [1] 371
