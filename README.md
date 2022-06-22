Becoming the Very Best API: Contacting the pokeAPI
================
Supro Debnath
2022-06-18

-   [Requirements](#requirements)
-   [pokeAPI Functions](#pokeapi-functions)
    -   [`Game Version`](#game-version)
    -   [`Generation Pokedex`](#generation-pokedex)
    -   [`TypeDex`](#typedex)
    -   [`Region`](#region)
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
library(ggpubr)
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
    stats <- sapply(pokeCON$stats, function(stats) stats[["base_stat"]])
    names(stats) <- sapply(pokeCON$stats, function(stats) stats[["stat"]][["name"]])
    
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

``` r
gendex(3)
```

    ##          name pokedex_id             type       ability weight  hp attack defense special.attack
    ## 1     treecko        252            grass      overgrow     50  40     45      35             65
    ## 2     grovyle        253            grass      overgrow    216  50     65      45             85
    ## 3    sceptile        254            grass      overgrow    522  70     85      65            105
    ## 4     torchic        255             fire         blaze     25  45     60      40             70
    ## 5   combusken        256    fire/fighting         blaze    195  60     85      60             85
    ## 6    blaziken        257    fire/fighting         blaze    520  80    120      70            110
    ## 7      mudkip        258            water       torrent     76  50     70      50             50
    ## 8   marshtomp        259     water/ground       torrent    280  70     85      70             60
    ## 9    swampert        260     water/ground       torrent    819 100    110      90             85
    ## 10  poochyena        261             dark      run-away    136  35     55      35             30
    ## 11  mightyena        262             dark    intimidate    370  70     90      70             60
    ## 12  zigzagoon        263           normal        pickup    175  38     30      41             30
    ## 13    linoone        264           normal        pickup    325  78     70      61             50
    ## 14    wurmple        265              bug   shield-dust     36  45     45      35             20
    ## 15    silcoon        266              bug     shed-skin    100  50     35      55             25
    ## 16  beautifly        267       bug/flying         swarm    284  60     70      50            100
    ## 17    cascoon        268              bug     shed-skin    115  50     35      55             25
    ## 18     dustox        269       bug/poison   shield-dust    316  60     50      70             50
    ## 19      lotad        270      water/grass    swift-swim     26  40     30      30             40
    ## 20     lombre        271      water/grass    swift-swim    325  60     50      50             60
    ## 21   ludicolo        272      water/grass    swift-swim    550  80     70      70             90
    ## 22     seedot        273            grass   chlorophyll     40  40     40      50             30
    ## 23    nuzleaf        274       grass/dark   chlorophyll    280  70     70      40             60
    ## 24    shiftry        275       grass/dark   chlorophyll    596  90    100      60             90
    ## 25    taillow        276    normal/flying          guts     23  40     55      30             30
    ## 26    swellow        277    normal/flying          guts    198  60     85      60             75
    ## 27    wingull        278     water/flying      keen-eye     95  40     30      30             55
    ## 28   pelipper        279     water/flying      keen-eye    280  60     50     100             95
    ## 29      ralts        280    psychic/fairy   synchronize     66  28     25      25             45
    ## 30     kirlia        281    psychic/fairy   synchronize    202  38     35      35             65
    ## 31  gardevoir        282    psychic/fairy   synchronize    484  68     65      65            125
    ## 32    surskit        283        bug/water    swift-swim     17  40     30      32             50
    ## 33 masquerain        284       bug/flying    intimidate     36  70     60      62            100
    ## 34  shroomish        285            grass  effect-spore     45  60     40      60             40
    ## 35    breloom        286   grass/fighting  effect-spore    392  60    130      80             60
    ## 36    slakoth        287           normal        truant    240  60     60      60             35
    ## 37   vigoroth        288           normal  vital-spirit    465  80     80      80             55
    ## 38    slaking        289           normal        truant   1305 150    160     100             95
    ## 39    nincada        290       bug/ground compound-eyes     55  31     45      90             30
    ## 40    ninjask        291       bug/flying   speed-boost    120  61     90      45             50
    ## 41   shedinja        292        bug/ghost  wonder-guard     12   1     90      45             30
    ## 42    whismur        293           normal    soundproof    163  64     51      23             51
    ## 43    loudred        294           normal    soundproof    405  84     71      43             71
    ## 44    exploud        295           normal    soundproof    840 104     91      63             91
    ## 45   makuhita        296         fighting     thick-fat    864  72     60      30             20
    ## 46   hariyama        297         fighting     thick-fat   2538 144    120      60             40
    ## 47    azurill        298     normal/fairy     thick-fat     20  50     20      40             20
    ## 48   nosepass        299             rock        sturdy    970  30     45     135             45
    ## 49     skitty        300           normal    cute-charm    110  50     45      45             35
    ## 50   delcatty        301           normal    cute-charm    326  70     65      65             55
    ## 51    sableye        302       dark/ghost      keen-eye    110  50     75      75             65
    ## 52     mawile        303      steel/fairy  hyper-cutter    115  50     85      85             55
    ## 53       aron        304       steel/rock        sturdy    600  50     70     100             40
    ## 54     lairon        305       steel/rock        sturdy   1200  60     90     140             50
    ## 55     aggron        306       steel/rock        sturdy   3600  70    110     180             60
    ## 56   meditite        307 fighting/psychic    pure-power    112  30     40      55             40
    ## 57   medicham        308 fighting/psychic    pure-power    315  60     60      75             60
    ## 58  electrike        309         electric        static    152  40     45      40             65
    ## 59  manectric        310         electric        static    402  70     75      60            105
    ## 60     plusle        311         electric          plus     42  60     50      40             85
    ## 61      minun        312         electric         minus     42  60     40      50             75
    ## 62    volbeat        313              bug    illuminate    177  65     73      75             47
    ## 63   illumise        314              bug     oblivious    177  65     47      75             73
    ## 64    roselia        315     grass/poison  natural-cure     20  50     60      45            100
    ## 65     gulpin        316           poison   liquid-ooze    103  70     43      53             43
    ## 66     swalot        317           poison   liquid-ooze    800 100     73      83             73
    ## 67   carvanha        318       water/dark    rough-skin    208  45     90      20             65
    ## 68   sharpedo        319       water/dark    rough-skin    888  70    120      40             95
    ## 69    wailmer        320            water    water-veil   1300 130     70      35             70
    ## 70    wailord        321            water    water-veil   3980 170     90      45             90
    ## 71      numel        322      fire/ground     oblivious    240  60     60      40             65
    ## 72   camerupt        323      fire/ground   magma-armor   2200  70    100      70            105
    ## 73    torkoal        324             fire   white-smoke    804  70     85     140             85
    ## 74     spoink        325          psychic     thick-fat    306  60     25      35             70
    ## 75    grumpig        326          psychic     thick-fat    715  80     45      65             90
    ## 76     spinda        327           normal     own-tempo     50  60     60      60             60
    ## 77   trapinch        328           ground  hyper-cutter    150  45    100      45             45
    ## 78    vibrava        329    ground/dragon      levitate    153  50     70      50             50
    ## 79     flygon        330    ground/dragon      levitate    820  80    100      80             80
    ## 80     cacnea        331            grass     sand-veil    513  50     85      40             85
    ## 81   cacturne        332       grass/dark     sand-veil    774  70    115      60            115
    ## 82     swablu        333    normal/flying  natural-cure     12  45     40      60             40
    ## 83    altaria        334    dragon/flying  natural-cure    206  75     70      90             70
    ## 84   zangoose        335           normal      immunity    403  73    115      60             60
    ## 85    seviper        336           poison     shed-skin    525  73    100      60            100
    ## 86   lunatone        337     rock/psychic      levitate   1680  90     55      65             95
    ## 87    solrock        338     rock/psychic      levitate   1540  90     95      85             55
    ## 88   barboach        339     water/ground     oblivious     19  50     48      43             46
    ## 89   whiscash        340     water/ground     oblivious    236 110     78      73             76
    ## 90   corphish        341            water  hyper-cutter    115  43     80      65             50
    ##    special.defense speed
    ## 1               55    70
    ## 2               65    95
    ## 3               85   120
    ## 4               50    45
    ## 5               60    55
    ## 6               70    80
    ## 7               50    40
    ## 8               70    50
    ## 9               90    60
    ## 10              30    35
    ## 11              60    70
    ## 12              41    60
    ## 13              61   100
    ## 14              30    20
    ## 15              25    15
    ## 16              50    65
    ## 17              25    15
    ## 18              90    65
    ## 19              50    30
    ## 20              70    50
    ## 21             100    70
    ## 22              30    30
    ## 23              40    60
    ## 24              60    80
    ## 25              30    85
    ## 26              50   125
    ## 27              30    85
    ## 28              70    65
    ## 29              35    40
    ## 30              55    50
    ## 31             115    80
    ## 32              52    65
    ## 33              82    80
    ## 34              60    35
    ## 35              60    70
    ## 36              35    30
    ## 37              55    90
    ## 38              65   100
    ## 39              30    40
    ## 40              50   160
    ## 41              30    40
    ## 42              23    28
    ## 43              43    48
    ## 44              73    68
    ## 45              30    25
    ## 46              60    50
    ## 47              40    20
    ## 48              90    30
    ## 49              35    50
    ## 50              55    90
    ## 51              65    50
    ## 52              55    50
    ## 53              40    30
    ## 54              50    40
    ## 55              60    50
    ## 56              55    60
    ## 57              75    80
    ## 58              40    65
    ## 59              60   105
    ## 60              75    95
    ## 61              85    95
    ## 62              85    85
    ## 63              85    85
    ## 64              80    65
    ## 65              53    40
    ## 66              83    55
    ## 67              20    65
    ## 68              40    95
    ## 69              35    60
    ## 70              45    60
    ## 71              45    35
    ## 72              75    40
    ## 73              70    20
    ## 74              80    60
    ## 75             110    80
    ## 76              60    60
    ## 77              45    10
    ## 78              50    70
    ## 79              80   100
    ## 80              40    35
    ## 81              60    55
    ## 82              75    50
    ## 83             105    80
    ## 84              60    90
    ## 85              60    65
    ## 86              85    70
    ## 87              65    70
    ## 88              41    60
    ## 89              71    60
    ## 90              35    35
    ##  [ reached 'max' / getOption("max.print") -- omitted 45 rows ]

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

### `Region`

``` r
region <- function(reg) {
  if ((reg > 8) || (reg < 1)) {
    stop("This is a non-existent region. Please refine search or submit a numeric input from 1 to 796.")
  }
  
  respREG <- GET(paste0("https://pokeapi.co/api/v2/region/", reg))
  conREG <- content(respREG, "parsed")
  df_pokeREG <- do.call(rbind.data.frame, c(conREG$locations, stringsAsFactors = FALSE))
  
  
  locPULL <- function(url) {
    locCON <- content(GET(url), "parsed")
    areaURL <- locCON$areas[[1]]$url
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
  
  regLIST <- lapply(df_pokeREG$url, locPULL)
  regdex <- do.call(rbind.data.frame, c(regLIST, stringsAsFactors = FALSE))
#  locdex <- pokedex %>% arrange(pokedex_id)
  return(regdex)
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
kanto <- pokeAPI("gen", 1)
kanto <- kanto %>% mutate(totalstats = hp+attack+defense+special.attack+special.defense+speed)
head(kanto)
```

    ##         name pokedex_id         type  ability weight hp attack defense special.attack special.defense
    ## 1  bulbasaur          1 grass/poison overgrow     69 45     49      49             65              65
    ## 2    ivysaur          2 grass/poison overgrow    130 60     62      63             80              80
    ## 3   venusaur          3 grass/poison overgrow   1000 80     82      83            100             100
    ## 4 charmander          4         fire    blaze     85 39     52      43             60              50
    ## 5 charmeleon          5         fire    blaze    190 58     64      58             80              65
    ## 6  charizard          6  fire/flying    blaze    905 78     84      78            109              85
    ##   speed totalstats
    ## 1    45        318
    ## 2    60        405
    ## 3    80        525
    ## 4    65        309
    ## 5    80        405
    ## 6   100        534

``` r
plot1 <- top_n(kanto, n=15, totalstats) %>% ggplot(., aes(x=name, y=totalstats, fill=type)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle=45))
plot1
```

![](README_files/figure-gfm/kanto%20strongest-1.png)<!-- -->

``` r
plot2 <- ggplot(kanto, aes(x=totalstats)) + geom_histogram()
plot2
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/kanto%20type-1.png)<!-- -->

``` r
table1 <- table(kanto$type)
knitr::kable(table1, col.names = gsub("[.]", " ", names(c("Type Combo", "Count"))))
```

|                 |     |
|:----------------|----:|
| bug             |   3 |
| bug/flying      |   2 |
| bug/grass       |   2 |
| bug/poison      |   5 |
| dragon          |   2 |
| dragon/flying   |   1 |
| electric        |   6 |
| electric/flying |   1 |
| electric/steel  |   2 |
| fairy           |   2 |
| fighting        |   7 |
| fire            |  10 |
| fire/flying     |   2 |
| ghost/poison    |   3 |
| grass           |   1 |
| grass/poison    |   9 |
| grass/psychic   |   2 |
| ground          |   6 |
| ground/rock     |   2 |
| ice/flying      |   1 |
| ice/psychic     |   1 |
| normal          |  12 |
| normal/fairy    |   2 |
| normal/flying   |   8 |
| poison          |  10 |
| poison/flying   |   2 |
| poison/ground   |   2 |
| psychic         |   7 |
| psychic/fairy   |   1 |
| rock/flying     |   1 |
| rock/ground     |   4 |
| rock/water      |   4 |
| water           |  18 |
| water/fighting  |   1 |
| water/flying    |   1 |
| water/ice       |   3 |
| water/poison    |   2 |
| water/psychic   |   3 |

``` r
plot3 <- ggplot(kanto, aes(x=attack, y=defense)) + geom_point(stat='identity') + geom_smooth() + stat_cor(mapping = NULL, data = NULL, method = "pearson", alternative = "two.sided")
plot3
```

![](README_files/figure-gfm/attack%20v%20defense%20kanto-1.png)<!-- -->

``` r
plot4 <- ggplot(kanto, aes(x=type, y=totalstats, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16, position=position_jitter(0.2)) + coord_flip() + theme(legend.position = "none")
plot4
```

![](README_files/figure-gfm/kanto%20boxplot-1.png)<!-- -->

``` r
hoenn <- pokeAPI("gen", 3)
hoenn <- hoenn %>% mutate(totalstats = hp+attack+defense+special.attack+special.defense+speed, avg_total = (totalstats/6))
head(hoenn)
```

    ##        name pokedex_id          type  ability weight hp attack defense special.attack special.defense
    ## 1   treecko        252         grass overgrow     50 40     45      35             65              55
    ## 2   grovyle        253         grass overgrow    216 50     65      45             85              65
    ## 3  sceptile        254         grass overgrow    522 70     85      65            105              85
    ## 4   torchic        255          fire    blaze     25 45     60      40             70              50
    ## 5 combusken        256 fire/fighting    blaze    195 60     85      60             85              60
    ## 6  blaziken        257 fire/fighting    blaze    520 80    120      70            110              70
    ##   speed totalstats avg_total
    ## 1    70        310  51.66667
    ## 2    95        405  67.50000
    ## 3   120        530  88.33333
    ## 4    45        310  51.66667
    ## 5    55        405  67.50000
    ## 6    80        530  88.33333

``` r
plot5 <- top_n(hoenn, n=15, totalstats) %>% ggplot(., aes(x=name, y=totalstats, fill=type)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle=45))
plot5
```

![](README_files/figure-gfm/hoenn%20strongest-1.png)<!-- -->

``` r
plot6 <- ggplot(hoenn, aes(x=totalstats))  + geom_histogram()
plot6
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/hoenn%20type-1.png)<!-- -->

``` r
plot7 <- ggplot(hoenn, aes(x=attack, y=defense)) + geom_point(stat='identity') + geom_smooth() + stat_cor(mapping = NULL, data = NULL, method = "pearson", alternative = "two.sided")
plot7
```

![](README_files/figure-gfm/attack%20v%20defense%20hoenn-1.png)<!-- -->

``` r
plot8 <- ggplot(hoenn, aes(x=type, y=totalstats, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16, position=position_jitter(0.2)) + coord_flip() + theme(legend.position = "none")
plot8
```

![](README_files/figure-gfm/hoenn%20boxplot-1.png)<!-- -->
