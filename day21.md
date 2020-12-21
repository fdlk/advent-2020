— Day 21: Allergen Assessment —
================
Fleur Kelpin
Dec 20, 2020

    library(tidyverse)
    input <- tibble(line = read_lines("day21.txt")) %>%
      mutate(row = row_number()) %>%
      extract(line, c("ingredient", "allergen"), "(.*) \\(contains (.*)\\)") %>%
      separate_rows(allergen, sep = ", ") %>%
      separate_rows(ingredient, sep = " ")
    input

    ## # A tibble: 4,603 x 3
    ##    ingredient allergen   row
    ##    <chr>      <chr>    <int>
    ##  1 smfnh      nuts         1
    ##  2 svztk      nuts         1
    ##  3 rqqf       nuts         1
    ##  4 sfhvsx     nuts         1
    ##  5 xctnhp     nuts         1
    ##  6 bvn        nuts         1
    ##  7 krv        nuts         1
    ##  8 gkcplx     nuts         1
    ##  9 ngpq       nuts         1
    ## 10 hmhll      nuts         1
    ## # … with 4,593 more rows

# Part 1

> Each allergen is found in exactly one ingredient. Each ingredient
> contains zero or one allergen. Allergens aren’t always marked; when
> they’re listed (as in (contains nuts, shellfish) after an ingredients
> list), the ingredient that contains each listed allergen will be
> somewhere in the corresponding ingredients list. However, even if an
> allergen isn’t listed, the ingredient that contains that allergen
> could still be present: maybe they forgot to label it, or maybe it was
> labeled in a language you don’t know.

> Determine which ingredients cannot possibly contain any of the
> allergens in your list. How many times do any of those ingredients
> appear?

This was a joy to write in R! (Once I figured out which methods to
call.) The input is already tidied. This method splits a data frame on
row and then joins them back together to find the ingredients that are
listed in each of the rows.

    shared_ingredients <- function(df) {
      ingredient <- split(df, df$row) %>%
        Reduce(function(df1, df2) inner_join(df1, df2, by = "ingredient"), .) %>%
        { .$ingredient }
      tibble(
        allergen = df$allergen[[1]],
        ingredient = ingredient
      )
    }
    shared_ingredients(filter(input, allergen=="fish"))

    ## # A tibble: 2 x 2
    ##   allergen ingredient
    ##   <chr>    <chr>     
    ## 1 fish     ktlh      
    ## 2 fish     dqsbql

Then split the input on allergen and call the shared\_ingredients method
to find the suspect ingredients for each allergen

    suspect_ingredients <- split(input, input$allergen) %>%
      map_df(shared_ingredients)
    suspect_ingredients

    ## # A tibble: 19 x 2
    ##    allergen  ingredient
    ##    <chr>     <chr>     
    ##  1 dairy     gkcplx    
    ##  2 dairy     dqsbql    
    ##  3 dairy     ppdplc    
    ##  4 dairy     ktlh      
    ##  5 eggs      gkcplx    
    ##  6 fish      ktlh      
    ##  7 fish      dqsbql    
    ##  8 nuts      dqsbql    
    ##  9 nuts      mvqkdj    
    ## 10 nuts      msfmt     
    ## 11 nuts      hbhsx     
    ## 12 sesame    dqsbql    
    ## 13 sesame    gkcplx    
    ## 14 shellfish ktlh      
    ## 15 shellfish mvqkdj    
    ## 16 soy       msfmt     
    ## 17 soy       ggsz      
    ## 18 wheat     hbhsx     
    ## 19 wheat     ktlh

Finally count the occurrences of the safe ingredients on rows

    input %>%
      filter(!ingredient %in% suspect_ingredients$ingredient) %>%
      group_by(ingredient) %>%
      summarize(row) %>%
      unique() %>%
      nrow()

    ## `summarise()` regrouping output by 'ingredient' (override with `.groups` argument)

    ## [1] 2098

# Part 2

> Now that you’ve isolated the inert ingredients, you should have enough
> information to figure out which ingredient contains which allergen.

Keep filtering out the suspect ingredients that match exactly one
allergen.

    dangerous_ingredients <-
      tibble(ingredient = character(), allergen = character())

    while (nrow(suspect_ingredients) > 0) {
      suspect_ingredients <-
        suspect_ingredients %>%
        filter(!ingredient %in% dangerous_ingredients$ingredient) %>%
        filter(!allergen %in% dangerous_ingredients$allergen)
      discovered <- suspect_ingredients %>%
        group_by(allergen) %>%
        filter(length(ingredient) == 1)
      dangerous_ingredients <- union(dangerous_ingredients, discovered)
    }

    dangerous_ingredients

    ## # A tibble: 8 x 2
    ## # Groups:   allergen [8]
    ##   ingredient allergen 
    ##   <chr>      <chr>    
    ## 1 gkcplx     eggs     
    ## 2 dqsbql     sesame   
    ## 3 ktlh       fish     
    ## 4 ppdplc     dairy    
    ## 5 mvqkdj     shellfish
    ## 6 hbhsx      wheat    
    ## 7 msfmt      nuts     
    ## 8 ggsz       soy

> Arrange the ingredients alphabetically by their allergen and separate
> them by commas to produce your canonical dangerous ingredient list.

    dangerous_ingredients %>%
      arrange(allergen) %>%
      { .$ingredient } %>%
      str_flatten(",")

    ## [1] "ppdplc,gkcplx,ktlh,msfmt,dqsbql,mvqkdj,ggsz,hbhsx"
