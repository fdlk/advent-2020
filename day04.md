— Day 4: Passport Processing —
================
Fleur Kelpin
Dec 3, 2020

    library(tidyverse)
    input <- readr::read_file("day04.txt") %>%
      str_split("\n\n") %>%
      unlist()
    input <- tibble(line = input)

    passports <- input %>%
      extract("line",
        into = "byr", regex = "byr:([^\\s]+)", remove = FALSE, convert = TRUE
      ) %>%
      extract("line",
        into = "iyr", regex = "iyr:([^\\s]+)", remove = FALSE, convert = TRUE
      ) %>%
      extract("line",
        into = "eyr", regex = "eyr:([^\\s]+)", remove = FALSE, convert = TRUE
      ) %>%
      extract("line", into = "hgt", regex = "hgt:([^\\s]+)", remove = FALSE) %>%
      extract("line", into = "hcl", regex = "hcl:([^\\s]+)", remove = FALSE) %>%
      extract("line", into = "ecl", regex = "ecl:([^\\s]+)", remove = FALSE) %>%
      extract("line", into = "pid", regex = "pid:([^\\s]+)", remove = FALSE) %>%
      extract("line", into = "cid", regex = "cid:([^\\s]+)")
    passports

    ## # A tibble: 282 x 8
    ##    cid   pid       ecl   hcl     hgt     eyr   iyr   byr
    ##    <chr> <chr>     <chr> <chr>   <chr> <int> <int> <int>
    ##  1 <NA>  662406624 amb   #cfa07d 150cm  2024  2015  1947
    ##  2 102   018128535 gry   #ceb3a1 182cm  2027  2013  1997
    ##  3 <NA>  916315544 oth   #733820 61in     NA  2014    NA
    ##  4 132   726519569 grn   #a97842 184cm  2026  2011  1980
    ##  5 69    619743219 grn   #6b5442 176cm  2027  2012  1980
    ##  6 <NA>  982796633 brn   #602927 164cm  2020  2014  1969
    ##  7 <NA>  15115163  gmt   bfab0d  <NA>   2039  1987  2006
    ##  8 117   322719183 brn   #efcc98 176cm  2020  2010  1957
    ##  9 <NA>  838813262 blu   #38f7fd 178cm  2029  2019  1954
    ## 10 <NA>  242570886 amb   #18171d 192cm  2023  2020  1927
    ## # … with 272 more rows

# Part 1

Count the number of valid passports - those that have all required
fields. Treat cid as optional. **In your batch file, how many passports
are valid?**

    drop_na(passports, -cid) %>%
      count()

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   250

# Part 2

You can continue to ignore the cid field, but each other field has
strict rules about what values are valid for automatic validation:

-   byr (Birth Year) - four digits; at least 1920 and at most 2002.
-   iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-   eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-   hgt (Height) - a number followed by either cm or in:
    -   If cm, the number must be at least 150 and at most 193.
    -   If in, the number must be at least 59 and at most 76.
-   hcl (Hair Color) - a \# followed by exactly six characters 0-9 or
    a-f.
-   ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-   pid (Passport ID) - a nine-digit number, including leading zeroes.
-   cid (Country ID) - ignored, missing or not.

Count the number of **valid** passports - those that have all required
fields **and valid values**. Continue to treat cid as optional. **In
your batch file, how many passports are valid?**

    passports %>%
      extract(hgt,
        into = c("hgt", "hgt_dim"), regex = "^([\\d]+)(cm|in)$", convert = TRUE
      ) %>%
      mutate(hgt_dim = as_factor(hgt_dim)) %>%
      drop_na(-cid) %>%
      filter(between(byr, 1920, 2002),
             between(iyr, 2010, 2020),
             between(eyr, 2020, 2030),
             hgt_dim == "cm" & between(hgt, 150, 193) |
          hgt_dim == "in" & between(hgt, 59, 76),
          str_detect(hcl, "^#[0-9a-f]{6}$"),
          str_detect(ecl, "^amb|blu|brn|gry|grn|hzl|oth$"),
          str_detect(pid, "^[0-9]{9}$")) %>%
      count()

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   158
