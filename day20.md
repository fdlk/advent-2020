— Day 20: Jurassic Jigsaw —
================
Fleur Kelpin
Dec 20, 2020

    library(tidyverse)
    options(digits=22)
    input <- read_file("day20.txt") %>%
      str_split("\\n\\n") %>%
      unlist() %>%
      {tibble(line=.)} %>%
      extract(line, c("id", "tile", NA), "Tile (\\d+):\\n((.|\\n)*)$", convert = T) %>%
      mutate(num=row_number())
    input

    ## # A tibble: 144 x 3
    ##       id tile                                                                num
    ##    <int> <chr>                                                             <int>
    ##  1  3571 "##..##....\n..##..#..#\n#...##....\n.....#...#\n..........\n...…     1
    ##  2  2687 "#..##..###\n..........\n.....##..#\n.#.#..##.#\n#.#..#..##\n#..…     2
    ##  3  3049 "##.#.#.###\n....##.###\n....##....\n#.........\n#.#...#..#\n#..…     3
    ##  4  1597 "###..##...\n##.....###\n.#..#..#.#\n#.......#.\n#.#.......\n.#.…     4
    ##  5  1301 "#.##.#.###\n#........#\n....#.....\n#.....####\n.#....#.##\n...…     5
    ##  6  3259 "..#####.#.\n.....#...#\n#...#.....\n...##...#.\n.#..#..##.\n.#.…     6
    ##  7  3989 "#..#..####\n..........\n#........#\n.#....#...\n..........\n#..…     7
    ##  8  3803 "#.####...#\n.###.#..#.\n....##..#.\n#........#\n..##..#...\n###…     8
    ##  9  2897 ".#..##.#.#\n..#.#...##\n#........#\n###.#..#.#\n...#..#.##\n...…     9
    ## 10  2843 "##.###..##\n#.#.......\n#....#....\n#..##.##..\n.##..#.##.\n#..…    10
    ## # … with 134 more rows

    n <- 10
    tiles <- array(FALSE, dim=c(n, n,nrow(input)))
    for(i in 1:nrow(input)){
      tiles[,,i] <- input$tile[[i]] %>%
        str_split("\n") %>%
        unlist() %>%
        map(~ str_split(., "")) %>%
        unlist() %>%
        map_lgl(~ . == "#") %>%
        matrix(nrow=n, byrow=T)
    }
    tiles[,,1]

    ##        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
    ##  [1,]  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ##  [2,] FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
    ##  [3,]  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ##  [4,] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
    ##  [5,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [6,] FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ##  [7,]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE
    ##  [8,]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
    ##  [9,]  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE
    ## [10,]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE

    edges <- function(x) {
      c(tiles[1,,x],
        tiles[,1,x],
        tiles[10,,x],
        tiles[,10,x],
        tiles[1,10:1,x],
        tiles[10:1,1,x],
        tiles[10,10:1,x],
        tiles[10:1,10,x]) %>%
      matrix(ncol=n, byrow=T)
    }
    edges(2)

    ##       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
    ## [1,]  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
    ## [2,]  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
    ## [3,] FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE
    ## [4,]  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE
    ## [5,]  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE
    ## [6,] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE
    ## [7,]  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE
    ## [8,]  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE

    matches <- function(i,j) {
      matrix(t(c(t(edges(i)),t(edges(j)))), ncol=n, byrow=T) %>%
        duplicated() %>%
        any()
    }
    num_matches <- function(i) {
      result <- 0
      for(j in 1:nrow(input)) {
        if(j != i && matches(i,j)){
          result <- result + 1
        }
      }
      result
    }
    input %>%
      rowwise() %>%
      mutate(matches = num_matches(num)) %>%
      filter(matches == 2) %>%
      ungroup() %>%
      summarise(prod(id))

    ## # A tibble: 1 x 1
    ##      `prod(id)`
    ##           <dbl>
    ## 1 8425574315321
