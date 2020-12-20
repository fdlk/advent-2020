— Day 20: Jurassic Jigsaw —
================
Fleur Kelpin
Dec 20, 2020

    library(tidyverse)
    options(digits = 22)
    input <- read_file("day20.txt") %>%
      str_split("\\n\\n") %>%
      unlist() %>%
      {
        tibble(line = .)
      } %>%
      extract(line, c("id", "tile", NA), "Tile (\\d+):\\n((.|\\n)*)$", convert = T) %>%
      mutate(num = row_number())
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
    tiles <- array(FALSE, dim = c(n, n, nrow(input)))
    for (i in 1:nrow(input)) {
      tiles[, , i] <- input$tile[[i]] %>%
        str_split("\n") %>%
        unlist() %>%
        map(~ str_split(., "")) %>%
        unlist() %>%
        map_lgl(~ . == "#") %>%
        matrix(nrow = n, byrow = T)
    }
    tiles[, , 1]

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
      c(
        tiles[1, , x],
        tiles[, 1, x],
        tiles[10, , x],
        tiles[, 10, x],
        tiles[1, 10:1, x],
        tiles[10:1, 1, x],
        tiles[10, 10:1, x],
        tiles[10:1, 10, x]
      ) %>%
        matrix(ncol = n, byrow = T)
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

    matches <- function(i, j) {
      matrix(t(c(t(edges(i)), t(edges(j)))), ncol = n, byrow = T) %>%
        duplicated() %>%
        any()
    }
    num_matches <- function(i) {
      result <- 0
      for (j in 1:nrow(input)) {
        if (j != i && matches(i, j)) {
          result <- result + 1
        }
      }
      result
    }
    corners <- input %>%
      rowwise() %>%
      filter(num_matches(num) == 2) %>%
      ungroup()

    summarise(corners, prod(id))

    ## # A tibble: 1 x 1
    ##      `prod(id)`
    ##           <dbl>
    ## 1 8425574315321

# Part 2

    neighbors <- matrix(FALSE, nrow = nrow(input), ncol = nrow(input))
    for (i in 1:nrow(input)) {
      for (j in 1:nrow(input)) {
        neighbors[i, j] <- i != j & matches(i, j)
      }
    }

    input <- input %>%
      rowwise() %>%
      mutate(nb <- list(which(neighbors[num, ])))
    input

    ## # A tibble: 144 x 4
    ## # Rowwise: 
    ##       id tile                                      num `nb <- list(which(neighb…
    ##    <int> <chr>                                   <int> <list>                   
    ##  1  3571 "##..##....\n..##..#..#\n#...##....\n.…     1 <int [4]>                
    ##  2  2687 "#..##..###\n..........\n.....##..#\n.…     2 <int [4]>                
    ##  3  3049 "##.#.#.###\n....##.###\n....##....\n#…     3 <int [4]>                
    ##  4  1597 "###..##...\n##.....###\n.#..#..#.#\n#…     4 <int [4]>                
    ##  5  1301 "#.##.#.###\n#........#\n....#.....\n#…     5 <int [4]>                
    ##  6  3259 "..#####.#.\n.....#...#\n#...#.....\n.…     6 <int [4]>                
    ##  7  3989 "#..#..####\n..........\n#........#\n.…     7 <int [3]>                
    ##  8  3803 "#.####...#\n.###.#..#.\n....##..#.\n#…     8 <int [4]>                
    ##  9  2897 ".#..##.#.#\n..#.#...##\n#........#\n#…     9 <int [4]>                
    ## 10  2843 "##.###..##\n#.#.......\n#....#....\n#…    10 <int [4]>                
    ## # … with 134 more rows

There are a lot of possible ways to lay this puzzle. Once we pick the
top left and the tile to the right of that, we can figure out the layout
of the whole grid

    m <- sqrt(nrow(input))
    puzzle <- matrix(NA_integer_, nrow = m, ncol = m)
    puzzle[1, 1] <- corners$num[[1]]
    puzzle[1, 2] <- which(neighbors[puzzle[1, 1], ])[[1]]
    for (j in 3:m) {
      candidates <- which(neighbors[puzzle[1, j - 1], ])
      edge <- map_lgl(candidates, ~ num_matches(.) < 4 & !. %in% puzzle)
      puzzle[1, j] <- candidates[which(edge)[[1]]]
    }
    for (i in 2:m) {
      candidates <- which(neighbors[puzzle[i - 1, 1], ])
      edge <- map_lgl(candidates, ~ num_matches(.) < 4 & !. %in% puzzle)
      puzzle[i, 1] <- candidates[which(edge)[[1]]]
      for (j in 2:m) {
        left <- puzzle[i, j - 1]
        top <- puzzle[i - 1, j]
        candidates <- which(neighbors[left, ] & neighbors[top, ])
        puzzle[i, j] <- candidates[!(candidates %in% puzzle)][[1]]
      }
    }
    puzzle

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
    ##  [1,]  123  120   57   89   77   65   38   31  133   136    93   139
    ##  [2,]  143    8   72   79  101  112   39    6  138    76    63    23
    ##  [3,]    7   14  114   19    1  110   55  125  134    71    75    34
    ##  [4,]  115  118   22   60   42   94    9   28  121    73   102   126
    ##  [5,]   27   59   78  116   58  144   24   86   64    50   140   127
    ##  [6,]   33   35   91   82   84   99   25   54   32   129   141    17
    ##  [7,]   53  117   10   98  113   49  128  107   16    83     4    47
    ##  [8,]   45  111    3  122   12   85   62   74   69    61   104    26
    ##  [9,]  137   40   20   36   56   18  108   90   97    52    70    95
    ## [10,]   11   96   46   44   43   92   13   81  131    87    68    88
    ## [11,]   29   41    5  100   15   51  103  124    2   105   130    67
    ## [12,]  135   30   80   37  106   21   48  119  109   132    66   142

Flip them tiles!

    flip <- function(tile) {
      n <- dim(tile)[[1]]
      result <- array(dim = c(n, n, 8))
      result[, , 1] <- tile
      result[, , 2] <- t(tile)
      result[, , 3] <- tile[, n:1]
      result[, , 4] <- t(tile[, n:1])
      result[, , 5] <- tile[n:1, ]
      result[, , 6] <- t(tile[n:1, ])
      result[, , 7] <- tile[n:1, n:1]
      result[, , 8] <- t(tile[n:1, n:1])
      result
    }

    shared_edges <- function(i, j) {
      e <- matrix(t(c(t(edges(i)), t(edges(j)))), ncol = n, byrow = T)
      e[duplicated(e), ]
    }
    shared_edges(123, 120)

    ##      [,1] [,2]  [,3]  [,4] [,5] [,6]  [,7]  [,8] [,9] [,10]
    ## [1,] TRUE TRUE  TRUE FALSE TRUE TRUE  TRUE FALSE TRUE  TRUE
    ## [2,] TRUE TRUE FALSE  TRUE TRUE TRUE FALSE  TRUE TRUE  TRUE

    orientation_rb <- function(i, j) {
      piece <- puzzle[i, j]
      right <- puzzle[i, j + 1]
      bottom <- puzzle[i + 1, j]
      right_edges <- shared_edges(piece, right)
      bottom_edges <- shared_edges(piece, bottom)
      tile <- tiles[, , piece]
      flips <- flip(tile)
      right_edge_flips <- t(flips[, 10, ])
      bottom_edge_flips <- t(flips[10, , ])
      flip_index <- which(map_lgl(1:8, ~
      (all(right_edge_flips[., ] == right_edges[1, ]) |
        all(right_edge_flips[., ] == right_edges[2, ])) &
        (all(bottom_edge_flips[., ] == bottom_edges[1, ]) |
          all(bottom_edge_flips[., ] == bottom_edges[2, ]))))
      flips[, , flip_index]
    }
    orientation_rb(1, 1)

    ##        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
    ##  [1,] FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ##  [2,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    ##  [3,] FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
    ##  [4,]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE
    ##  [5,] FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE
    ##  [6,]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    ##  [7,]  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ##  [8,] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ##  [9,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## [10,] FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE

    orientation_l <- function(i, j, left_edge) {
      piece <- puzzle[i, j]
      left <- puzzle[i, j - 1]
      left_edges <- shared_edges(piece, left)
      tile <- tiles[, , piece]
      flips <- flip(tile)
      left_edge_flips <- t(flips[, 1, ])
      flip_index <- which(map_lgl(1:8, ~ all(left_edge_flips[., ] == left_edge)))
      flips[, , flip_index]
    }
    orientation_t <- function(i, j, top_edge) {
      piece <- puzzle[i, j]
      top <- puzzle[i - 1, j]
      top_edges <- shared_edges(piece, top)
      tile <- tiles[, , piece]
      flips <- flip(tile)
      top_edge_flips <- t(flips[1, , ])
      flip_index <- which(map_lgl(1:8, ~ all(top_edge_flips[., ] == top_edge)))
      flips[, , flip_index]
    }

    map_with_stitches <- matrix(nrow = m * (n - 1) + 1, ncol = m * (n - 1) + 1)
    for (i in 1:m) {
      for (j in 1:m) {
        top_left <- c((i - 1) * 9, (j - 1) * 9)
        if (j < m & i < m) {
          tile <- orientation_rb(i, j)
        } else if (j == m & i < m) {
          left_edge <- map_with_stitches[1:10 + top_left[[1]], 1 + top_left[[2]]]
          tile <- orientation_l(i, j, left_edge)
        } else {
          top_edge <- map_with_stitches[1 + top_left[[1]], 1:10 + top_left[[2]]]
          tile <- orientation_t(i, j, top_edge)
        }
        map_with_stitches[1:10 + top_left[[1]], 1:10 + top_left[[2]]] <- tile
      }
    }
    map <- matrix(nrow = m * (n - 2), ncol = m * (n - 2))
    for (i in 1:m) {
      for (j in 1:m) {
        top_left_from <- c((i - 1) * 9, (j - 1) * 9)
        top_left_to <- c((i - 1) * 8, (j - 1) * 8)
        map[
          1:8 + top_left_to[[1]],
          1:8 + top_left_to[[2]]
        ] <-
          map_with_stitches[
            2:9 + top_left_from[[1]],
            2:9 + top_left_from[[2]]
          ]
      }
    }

Now go hunting for the monsters!

    monster <- read_lines("day20-monster.txt") %>%
      map(~ str_split(., "")) %>%
      unlist() %>%
      map_lgl(~ . == "#") %>%
      matrix(nrow = 3, byrow = T)

    remove_monster <- function(i, j, map) {
      if (all((map[
        i + 1:nrow(monster) - 1,
        j + 1:ncol(monster) - 1
      ] & monster) == monster)) {
        map[i + 1:nrow(monster) - 1, j + 1:ncol(monster) - 1] <-
          map[i + 1:nrow(monster) - 1, j + 1:ncol(monster) - 1] & !monster
        TRUE
      } else {
        FALSE
      }
    }

    flipped_maps <- flip(map)
    for (k in 1:8) {
      found_monsters <- FALSE
      for (i in 1:(dim(flipped_maps)[[1]] - dim(monster)[[1]])) {
        for (j in 1:(dim(flipped_maps)[[2]] - dim(monster)[[2]])) {
          if (all((flipped_maps[
            i + 1:nrow(monster) - 1,
            j + 1:ncol(monster) - 1,
            k
          ] & monster) == monster)) {
            flipped_maps[i + 1:nrow(monster) - 1, j + 1:ncol(monster) - 1, k] <-
              flipped_maps[i + 1:nrow(monster) - 1, j + 1:ncol(monster) - 1, k] & !monster
            found_monsters <- TRUE
          }
        }
      }
      if (found_monsters) {
        print(k)
        print(sum(flipped_maps[, , k]))
      }
    }

    ## [1] 3
    ## [1] 1841
