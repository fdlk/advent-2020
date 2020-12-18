— Day 18: Operation Order —
================
Fleur Kelpin
Dec 18, 2020

    library(tidyverse)
    library(Ramble)
    input <- readLines("day18.txt") %>%
      map_chr(~ str_remove_all(., "\\s"))
    tibble(input)

    ## # A tibble: 374 x 1
    ##    input                                                                        
    ##    <chr>                                                                        
    ##  1 (9+4*9*4)+3+7*8                                                              
    ##  2 4*4*(8+4*2*3+(6+6*7+6)+(7+4*9*7+2))                                          
    ##  3 6+((8+2)*(2*6*9*6*5))+5                                                      
    ##  4 9*6+((7*6*8*2+9*5)+(3+2*8*5+4)*8*6+8+6)+8+(7*(9+7+3*5+7+8)+(6*7+8+9+7)*5*(7+…
    ##  5 2*(7+4+7*(8+9)+5*3)*4+(9*4*5)                                                
    ##  6 (9*9+9*7+(8+4*6)+(9+3+9+7*5+2))*3                                            
    ##  7 9*7+4+((4*9)*4*(7+7*8*3)*(6*7*7*4+9*8))*4*8                                  
    ##  8 4+9*2+(2+(6*5*6)+3*4+6)                                                      
    ##  9 6+7+(6*(3+8*6*8)*(2+6*5+6+5)*(6+3+3+3+8*6))*(9*2*(8+6*3+8*9))+5              
    ## 10 (7*8+3*2*8*(7*6+3*4))+(5+3+3+3+(6+5*9+7*7+5))+(7*9*(5*9)+3+(3*3*4*9*3)+5)+6  
    ## # … with 364 more rows

# Part 1

Evaluate the expression on each line of the homework; what is the sum of
the resulting values?

Writing a grammar for this is trickier than I thought, because
associativity matters and it’s easy to get circular references and
infinite recursion.

    expr: block (plusOrTimes block)*
    block: number | '(' expr ')'

Then when resolving the expressions, we’ll get a list of integers and
operators.

    resolve <- function(x) {
      result <- as.numeric(x[[1]])
      if (length(x) <= 2) {
        return(result)
      }
      for (i in seq(from = 2, to = length(x), by = 2)) {
        result <- switch(x[[i]],
          "+" = result + as.numeric(x[[i + 1]]),
          "*" = result * as.numeric(x[[i + 1]])
        )
      }
      result
    }
    resolve(c("2", "+", "4", "*", "4"))

    ## [1] 24

Now we can use this resolve function in a parser combinator:

    library(Ramble)

    pPlusOrTimes <- symbol("+") %alt% symbol("*")
    pExpr <- (pBlock %then% many(pPlusOrTimes %then% pBlock)) %using%
      function(x) {
        resolve(unlist(c(x)))
      }
    pBlock <- (natural() %using% function(x) {
      as.numeric(unlist(c(x)))
    }) %alt%
      ((symbol("(") %then%
        pExpr %then%
        symbol(")")) %using% function(x) {
        unlist(c(x))[[2]]
      })

    pExpr("1+(2+4*4)")

    ## $result
    ## [1] 25
    ## 
    ## $leftover
    ## [1] ""

And finally compute the answer

    input %>%
      map_dbl(~ pExpr(.)$result) %>%
      sum() %>%
      format(scientific = F)

    ## [1] "67800526776934"

# Part 2

Now, addition and multiplication have different precedence levels, but
they’re not the ones you’re familiar with. Instead, addition is
evaluated before multiplication.

That should actually be easier cause associativity no longer matters and
we can evaluate everything eagerly.

    expr :: = factor * expr | factor
    factor :: = term + expr | term
    term :: = (expr) | digit+

    pExpr <- ((pFactor %then%
      symbol("*") %then%
      pExpr %using% function(x) {
        prod(as.numeric(unlist(c(x))[c(1, 3)]))
      }) %alt% pFactor)
    pFactor <- ((pTerm %then%
      symbol("+") %then%
      pFactor %using% function(x) {
        sum(as.numeric(unlist(c(x))[c(1, 3)]))
      }) %alt% pTerm)
    pTerm <- ((symbol("(") %then%
      pExpr %then%
      symbol(")") %using% function(x) {
        return(as.numeric(unlist(c(x))[2]))
      }) %alt% natural())
    pExpr("4*4*(8+4*2*3+(6+6*7+6)+(7+4*9*7+2))")

    ## $result
    ## [1] 403200
    ## 
    ## $leftover
    ## [1] ""

And the answer

    input %>%
      map_dbl(~ pExpr(.)$result) %>%
      sum() %>%
      format(scientific = F)

    ## [1] "340789638435483"
