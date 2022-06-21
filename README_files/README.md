Becoming the Very Best API: Contacting the pokeAPI
================
Supro Debnath
2022-06-18

-   [Requirements](#requirements)
-   [pokeAPI Functions](#pokeapi-functions)
    -   [`Game Version`](#game-version)
    -   [`Generation Pokedex`](#generation-pokedex)
    -   [`TypeDex`](#typedex)
    -   [`pokeAPI Wrapper Function`](#pokeapi-wrapper-function)

This vignette is a step-by-step guide to interacting with the
[pokeAPI](https://pokeapi.co/docs/v2#info). There are a few functions in
here that explore the data present, as well as some visualizations that
highlight interesting patterns in the world of pokemon. Most of the ids
are represented as numeric (i.e. `Generation 1` referring to the Kanto
pokemon, or `Version 8` referring to the Hoenn region). For that reason,
if you intend on using any of these functions, please be mindful of how
the function inputs correspond to the data you are trying to pull.

## Requirements

To interact with this API, a few packages must be installed. \*
`jsonlite`: Works with JSON in R and is useful for parsing data and
interacting with a web API. \* `httr`: Provides useful tools for working
with HTTP. \* `tidyverse`: Collection of R packages that contribute
heavily to API interaction and data visualization. \* `knitr`: Provides
tools for dynamic reporting in R. \* `dplyr`: Package that provides
tools for working with data frames. \* `ggplot2`: Powerful for complex
data visualizations.

``` r
library(jsonlite)
library(httr)
library(tidyverse)
library(knitr)
library(dplyr)
library(ggplot2)
```

## pokeAPI Functions

This section is dedicated to the functions I’ve created to pull data
from this API.

### `Game Version`

This function pulls the relevant version information of any existing
game from the `version` endpoint. It returns a `tibble` listing the name
of the game, the region, and which generation it falls into. It takes a
numeric input from `1` to `34`, which is the order of each game being
introduced.

``` r
version <- function(game) {
  # Checks if a valid input has been entered into the function.
  if ((game > 34) || (game < 1)) {
    stop("This is an invalid game. Please refine search.")
  }
  
  #Set URL for the version endpoint in the API.
  respGAME <- GET(paste0("http://pokeapi.co/api/v2/version/", game))
  conGAME <- content(respGAME)
  ver <- conGAME$version_group$url %>% GET() %>% content()
  
  #Create a tibble that prints basic version information based on the function input.
  output <- tibble(
    name = ver$name,
    region = ifelse(is_empty(ver$region), NA, ver$regions[[1]]$name),
    generation = ver$generation$name
  )
  return(output)
}
```

### `Generation Pokedex`

This function returns a complete pokedex from the `generation` endpoint.
It returns a `data.frame` of pokemon found in that generation, listing
their `name`, `id`, `typing` (i.e. fighting type or bug/flying type),
`ability` (returning the primary ability, not all pokemon have hidden
abilities), and their `stats` (attack, speed, etc.). This function takes
a numeric input from `1` to `8`, which is the order of each generation
being introduced.

``` r
gendex <- function(gen) {
  if ((gen > 8) || (gen < 1)) {
    stop("This is a non-existent generation. Please refine search or submit a numeric input from 1 to 8.")
  }
  
  respGEN <- GET(paste0("http://pokeapi.co/api/v2/generation/", gen))
  conGEN <- content(respGEN, "parsed")
  df_pokeGEN <- do.call(rbind.data.frame, c(conGEN$pokemon_species, stringsAsFactors = FALSE))
  
  
  pokePULL <- function(url) {
    specCON <- content(GET(url), "parsed")
    pokeURL <- specCON$varieties[[1]]$pokemon$url
    pokeCON <- content(GET(pokeURL), "parsed")
    stats <-
      sapply(pokeCON$stats, function(stats)
        stats[["base_stat"]])
    names(stats) <-
      sapply(pokeCON$stats, function(stats)
        stats[["stat"]][["name"]])
    
    for (i in seq_along(pokeCON$types)) {
      if (pokeCON$types[[i]][["slot"]] == 1){
        type <- pokeCON$types[[1]][["type"]][["name"]]
      }
      else {
        type <- paste0(pokeCON$types[[1]][["type"]][["name"]], "/",      
                       pokeCON$types[[2]][["type"]][["name"]])
      }
    }
    
    for (i in seq_along(pokeCON$abilities)) {
      if (pokeCON$abilities[[i]][["slot"]] == 1) {
        ability <- pokeCON$abilities[[1]][["ability"]][["name"]]
      } 
    }
    
    return(
      c(
        name = pokeCON$name,
        pokedex_id = pokeCON$id,
        type = type,
        ability = ability,
        weight = pokeCON$weight,
        as.list(stats)
      )
    )
  }
  
  pokeLIST <- lapply(df_pokeGEN$url, pokePULL)
  pokedex <- do.call(rbind.data.frame, c(pokeLIST, stringsAsFactors = FALSE))
  pokedex <- pokedex %>% arrange(pokedex_id)
  return(pokedex)
}
```

### `TypeDex`

This function returns a complete list of moves from the `type` endpoint.
It returns a `data.frame` of moves found in that type, listing their
`name`, `id`, `accuracy`, `power`, `pp` (how many times the move can be
used), `crit` (whether it can critically strike), `drain` (whether the
move drains health from the opponent), and `flinch` (the chance the move
causes the enemy to flinch. This function takes a numeric input from `1`
to `20`, which is the number of types present.

``` r
typedex <- function(type) {
  if ((type > 20) || (type < 1)) {
    stop("This is a non-existent typing. Please refine search or submit a numeric input from 1 to 20.")
  }
  
  respTYPE <- GET(paste0("http://pokeapi.co/api/v2/type/", type))
  conTYPE <- content(respTYPE, "parsed")
  df_typeGEN <- do.call(rbind.data.frame, c(conTYPE$moves, stringsAsFactors = FALSE))

  typePULL <- function(url) {
    moveCON <- content(GET(url), "parsed")
    
    
    if (is.null(moveCON$accuracy)) {
        moveCON$accuracy = "NA"
    }
    
    if (is.null(moveCON$power)) {
        moveCON$power = "NA"
    }
    
    return(
      c(
        name = moveCON$name,
        move_id = moveCON$id,
        accuracy = moveCON$accuracy,
        power = moveCON$power,
        pp = moveCON$pp,
        crit = moveCON$meta$crit_rate,
        drain = moveCON$meta$drain,
        flinch = moveCON$meta$flinch_chance
      )
    )
  }
  
  typeLIST <- lapply(df_typeGEN$url, typePULL)
  typedex <- do.call(rbind.data.frame, c(typeLIST, stringsAsFactors = FALSE))
  names(typedex) <- c('name', 'move_id', 'accuracy', 'power', 'pp', 'crit_rate', 'drain', 'flinch_chance')
  typedex <- type.convert(typedex)
  return(typedex)
}
```

``` r
location <- function(loc) {
  if ((loc > 796) || (loc < 1)) {
    stop("This is a non-existent location. Please refine search or submit a numeric input from 1 to 796.")
  }
  
  respLOC <- GET(paste0("https://pokeapi.co/api/v2/location/", loc))
  conLOC <- content(respLOC, "parsed")
  df_pokeLOC <- do.call(rbind.data.frame, c(conLOC$results, stringsAsFactors = FALSE))
  
  
  resPULL <- function(url) {
    resCON <- content(GET(url), "parsed")
    areaURL <- resCON$areas[[1]]$url
    areaCON <- content(GET(areaURL), "parsed")

    return(
      c(
        name = areaCON$name,
        area_id = areaCON$id,
        pokemon = areaCON$pokemon_encounters$pokemon$name,
        chance = areaCON$pokemon_encounters$version_details$encounter_details$chance,
        level = areaCON$pokemon_encounters$version_details$encounter_details$max_level
      )
    )
  }
  
  locLIST <- lapply(df_pokeLOC$url, resPULL)
  locdex <- do.call(rbind.data.frame, c(locLIST, stringsAsFactors = FALSE))
#  locdex <- pokedex %>% arrange(pokedex_id)
  return(locdex)
}
```

### `pokeAPI Wrapper Function`

``` r
pokeAPI <- function(func, ...){
  ###
  # This function is a wrapper for the other functions. It takes in the name
  # of the function to use as a character and any additional arguments for that
  # function.
  ###
  
  # Find and call the appropriate function using conditional logic.
  
  if (func == "version"){
    output <- version(...)
  }
  else if (func == "gen"){
    output <- gendex(...)
  }
  else if (func == "type"){
    output <- typedex(...)
  }
  else {
    stop("ERROR: Argument for func is not valid!")
  }
  
  # Return the output from the appropriate function.
  return(output)
}
```

``` r
typing <- pokeAPI("gen", 1)
typing <- typing %>% mutate(totalstats = hp+attack+defense+special.attack+special.defense+speed)
typing
```

    ##          name pokedex_id           type       ability weight  hp attack defense special.attack
    ## 1   bulbasaur          1   grass/poison      overgrow     69  45     49      49             65
    ## 2     ivysaur          2   grass/poison      overgrow    130  60     62      63             80
    ## 3    venusaur          3   grass/poison      overgrow   1000  80     82      83            100
    ## 4  charmander          4           fire         blaze     85  39     52      43             60
    ## 5  charmeleon          5           fire         blaze    190  58     64      58             80
    ## 6   charizard          6    fire/flying         blaze    905  78     84      78            109
    ## 7    squirtle          7          water       torrent     90  44     48      65             50
    ## 8   wartortle          8          water       torrent    225  59     63      80             65
    ## 9   blastoise          9          water       torrent    855  79     83     100             85
    ## 10   caterpie         10            bug   shield-dust     29  45     30      35             20
    ## 11    metapod         11            bug     shed-skin     99  50     20      55             25
    ## 12 butterfree         12     bug/flying compound-eyes    320  60     45      50             90
    ## 13     weedle         13     bug/poison   shield-dust     32  40     35      30             20
    ## 14     kakuna         14     bug/poison     shed-skin    100  45     25      50             25
    ## 15   beedrill         15     bug/poison         swarm    295  65     90      40             45
    ## 16     pidgey         16  normal/flying      keen-eye     18  40     45      40             35
    ## 17  pidgeotto         17  normal/flying      keen-eye    300  63     60      55             50
    ## 18    pidgeot         18  normal/flying      keen-eye    395  83     80      75             70
    ## 19    rattata         19         normal      run-away     35  30     56      35             25
    ## 20   raticate         20         normal      run-away    185  55     81      60             50
    ## 21    spearow         21  normal/flying      keen-eye     20  40     60      30             31
    ## 22     fearow         22  normal/flying      keen-eye    380  65     90      65             61
    ## 23      ekans         23         poison    intimidate     69  35     60      44             40
    ## 24      arbok         24         poison    intimidate    650  60     95      69             65
    ## 25    pikachu         25       electric        static     60  35     55      40             50
    ## 26     raichu         26       electric        static    300  60     90      55             90
    ## 27  sandshrew         27         ground     sand-veil    120  50     75      85             20
    ## 28  sandslash         28         ground     sand-veil    295  75    100     110             45
    ## 29  nidoran-f         29         poison  poison-point     70  55     47      52             40
    ## 30   nidorina         30         poison  poison-point    200  70     62      67             55
    ## 31  nidoqueen         31  poison/ground  poison-point    600  90     92      87             75
    ## 32  nidoran-m         32         poison  poison-point     90  46     57      40             40
    ## 33   nidorino         33         poison  poison-point    195  61     72      57             55
    ## 34   nidoking         34  poison/ground  poison-point    620  81    102      77             85
    ## 35   clefairy         35          fairy    cute-charm     75  70     45      48             60
    ## 36   clefable         36          fairy    cute-charm    400  95     70      73             95
    ## 37     vulpix         37           fire    flash-fire     99  38     41      40             50
    ## 38  ninetales         38           fire    flash-fire    199  73     76      75             81
    ## 39 jigglypuff         39   normal/fairy    cute-charm     55 115     45      20             45
    ## 40 wigglytuff         40   normal/fairy    cute-charm    120 140     70      45             85
    ## 41      zubat         41  poison/flying   inner-focus     75  40     45      35             30
    ## 42     golbat         42  poison/flying   inner-focus    550  75     80      70             65
    ## 43     oddish         43   grass/poison   chlorophyll     54  45     50      55             75
    ## 44      gloom         44   grass/poison   chlorophyll     86  60     65      70             85
    ## 45  vileplume         45   grass/poison   chlorophyll    186  75     80      85            110
    ## 46      paras         46      bug/grass  effect-spore     54  35     70      55             45
    ## 47   parasect         47      bug/grass  effect-spore    295  60     95      80             60
    ## 48    venonat         48     bug/poison compound-eyes    300  60     55      50             40
    ## 49   venomoth         49     bug/poison   shield-dust    125  70     65      60             90
    ## 50    diglett         50         ground     sand-veil      8  10     55      25             35
    ## 51    dugtrio         51         ground     sand-veil    333  35    100      50             50
    ## 52     meowth         52         normal        pickup     42  40     45      35             40
    ## 53    persian         53         normal        limber    320  65     70      60             65
    ## 54    psyduck         54          water          damp    196  50     52      48             65
    ## 55    golduck         55          water          damp    766  80     82      78             95
    ## 56     mankey         56       fighting  vital-spirit    280  40     80      35             35
    ## 57   primeape         57       fighting  vital-spirit    320  65    105      60             60
    ## 58  growlithe         58           fire    intimidate    190  55     70      45             70
    ## 59   arcanine         59           fire    intimidate   1550  90    110      80            100
    ## 60    poliwag         60          water  water-absorb    124  40     50      40             40
    ## 61  poliwhirl         61          water  water-absorb    200  65     65      65             50
    ## 62  poliwrath         62 water/fighting  water-absorb    540  90     95      95             70
    ## 63       abra         63        psychic   synchronize    195  25     20      15            105
    ## 64    kadabra         64        psychic   synchronize    565  40     35      30            120
    ## 65   alakazam         65        psychic   synchronize    480  55     50      45            135
    ## 66     machop         66       fighting          guts    195  70     80      50             35
    ## 67    machoke         67       fighting          guts    705  80    100      70             50
    ## 68    machamp         68       fighting          guts   1300  90    130      80             65
    ## 69 bellsprout         69   grass/poison   chlorophyll     40  50     75      35             70
    ## 70 weepinbell         70   grass/poison   chlorophyll     64  65     90      50             85
    ## 71 victreebel         71   grass/poison   chlorophyll    155  80    105      65            100
    ## 72  tentacool         72   water/poison    clear-body    455  40     40      35             50
    ## 73 tentacruel         73   water/poison    clear-body    550  80     70      65             80
    ## 74    geodude         74    rock/ground     rock-head    200  40     80     100             30
    ## 75   graveler         75    rock/ground     rock-head   1050  55     95     115             45
    ## 76      golem         76    rock/ground     rock-head   3000  80    120     130             55
    ## 77     ponyta         77           fire      run-away    300  50     85      55             65
    ## 78   rapidash         78           fire      run-away    950  65    100      70             80
    ## 79   slowpoke         79  water/psychic     oblivious    360  90     65      65             40
    ## 80    slowbro         80  water/psychic     oblivious    785  95     75     110            100
    ## 81  magnemite         81 electric/steel   magnet-pull     60  25     35      70             95
    ## 82   magneton         82 electric/steel   magnet-pull    600  50     60      95            120
    ## 83  farfetchd         83  normal/flying      keen-eye    150  52     90      55             58
    ##    special.defense speed totalstats
    ## 1               65    45        318
    ## 2               80    60        405
    ## 3              100    80        525
    ## 4               50    65        309
    ## 5               65    80        405
    ## 6               85   100        534
    ## 7               64    43        314
    ## 8               80    58        405
    ## 9              105    78        530
    ## 10              20    45        195
    ## 11              25    30        205
    ## 12              80    70        395
    ## 13              20    50        195
    ## 14              25    35        205
    ## 15              80    75        395
    ## 16              35    56        251
    ## 17              50    71        349
    ## 18              70   101        479
    ## 19              35    72        253
    ## 20              70    97        413
    ## 21              31    70        262
    ## 22              61   100        442
    ## 23              54    55        288
    ## 24              79    80        448
    ## 25              50    90        320
    ## 26              80   110        485
    ## 27              30    40        300
    ## 28              55    65        450
    ## 29              40    41        275
    ## 30              55    56        365
    ## 31              85    76        505
    ## 32              40    50        273
    ## 33              55    65        365
    ## 34              75    85        505
    ## 35              65    35        323
    ## 36              90    60        483
    ## 37              65    65        299
    ## 38             100   100        505
    ## 39              25    20        270
    ## 40              50    45        435
    ## 41              40    55        245
    ## 42              75    90        455
    ## 43              65    30        320
    ## 44              75    40        395
    ## 45              90    50        490
    ## 46              55    25        285
    ## 47              80    30        405
    ## 48              55    45        305
    ## 49              75    90        450
    ## 50              45    95        265
    ## 51              70   120        425
    ## 52              40    90        290
    ## 53              65   115        440
    ## 54              50    55        320
    ## 55              80    85        500
    ## 56              45    70        305
    ## 57              70    95        455
    ## 58              50    60        350
    ## 59              80    95        555
    ## 60              40    90        300
    ## 61              50    90        385
    ## 62              90    70        510
    ## 63              55    90        310
    ## 64              70   105        400
    ## 65              95   120        500
    ## 66              35    35        305
    ## 67              60    45        405
    ## 68              85    55        505
    ## 69              30    40        300
    ## 70              45    55        390
    ## 71              70    70        490
    ## 72             100    70        335
    ## 73             120   100        515
    ## 74              30    20        300
    ## 75              45    35        390
    ## 76              65    45        495
    ## 77              65    90        410
    ## 78              80   105        500
    ## 79              40    15        315
    ## 80              80    30        490
    ## 81              55    45        325
    ## 82              70    70        465
    ## 83              62    60        377
    ##  [ reached 'max' / getOption("max.print") -- omitted 68 rows ]

``` r
plot1 <- top_n(typing, n=15, totalstats) %>% ggplot(., aes(x=name, y=totalstats, fill=type)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle=45))
plot1
```

![](C:/Users/16787/OneDrive/Documents/pokeAPI-Vignette/README_files/README_files/figure-gfm/unnamed-chunk-99-1.png)<!-- -->

``` r
move1 <- pokeAPI("type", 14)
table1 <- table(move1$power)
knitr::kable(table1)
```

| Var1 | Freq |
|:-----|-----:|
| 10   |    1 |
| 20   |    1 |
| 50   |    1 |
| 60   |    1 |
| 65   |    1 |
| 70   |    3 |
| 80   |    7 |
| 85   |    1 |
| 90   |    2 |
| 100  |    3 |
| 120  |    2 |
| 140  |    1 |
| 160  |    1 |
| 185  |    1 |
| 200  |    1 |

``` r
move2 <- pokeAPI("type", 3)
table2 <- table(move2$power)
knitr::kable(table2)
```

| Var1 | Freq |
|:-----|-----:|
| 10   |    1 |
| 35   |    1 |
| 40   |    2 |
| 55   |    1 |
| 60   |    5 |
| 65   |    1 |
| 75   |    1 |
| 80   |    2 |
| 85   |    1 |
| 90   |    2 |
| 100  |    2 |
| 110  |    1 |
| 120  |    2 |
| 140  |    1 |

``` r
plot2 <- ggplot(typing, aes(x=attack, y=defense)) + geom_point(stat='identity') + geom_smooth() + theme(axis.text.x = element_text(angle=45))
plot2
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](C:/Users/16787/OneDrive/Documents/pokeAPI-Vignette/README_files/README_files/figure-gfm/unnamed-chunk-100-1.png)<!-- -->
