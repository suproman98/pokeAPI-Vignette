Becoming the Very Best API: Contacting the pokeAPI
================
Supro Debnath
2022-06-18

-   [Requirements](#requirements)
-   [pokeAPI Functions](#pokeapi-functions)
    -   [`Game Version`](#game-version)
    -   [`Generation Pokedex`](#generation-pokedex)
    -   [`Game Version`](#game-version-1)
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

``` r
version(8)
```

    ## # A tibble: 1 × 3
    ##   name          region generation    
    ##   <chr>         <chr>  <chr>         
    ## 1 ruby-sapphire hoenn  generation-iii

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

    ##           name pokedex_id             type       ability  hp attack defense special.attack special.defense
    ## 1      treecko        252            grass      overgrow  40     45      35             65              55
    ## 2      grovyle        253            grass      overgrow  50     65      45             85              65
    ## 3     sceptile        254            grass      overgrow  70     85      65            105              85
    ## 4      torchic        255             fire         blaze  45     60      40             70              50
    ## 5    combusken        256    fire/fighting         blaze  60     85      60             85              60
    ## 6     blaziken        257    fire/fighting         blaze  80    120      70            110              70
    ## 7       mudkip        258            water       torrent  50     70      50             50              50
    ## 8    marshtomp        259     water/ground       torrent  70     85      70             60              70
    ## 9     swampert        260     water/ground       torrent 100    110      90             85              90
    ## 10   poochyena        261             dark      run-away  35     55      35             30              30
    ## 11   mightyena        262             dark    intimidate  70     90      70             60              60
    ## 12   zigzagoon        263           normal        pickup  38     30      41             30              41
    ## 13     linoone        264           normal        pickup  78     70      61             50              61
    ## 14     wurmple        265              bug   shield-dust  45     45      35             20              30
    ## 15     silcoon        266              bug     shed-skin  50     35      55             25              25
    ## 16   beautifly        267       bug/flying         swarm  60     70      50            100              50
    ## 17     cascoon        268              bug     shed-skin  50     35      55             25              25
    ## 18      dustox        269       bug/poison   shield-dust  60     50      70             50              90
    ## 19       lotad        270      water/grass    swift-swim  40     30      30             40              50
    ## 20      lombre        271      water/grass    swift-swim  60     50      50             60              70
    ## 21    ludicolo        272      water/grass    swift-swim  80     70      70             90             100
    ## 22      seedot        273            grass   chlorophyll  40     40      50             30              30
    ## 23     nuzleaf        274       grass/dark   chlorophyll  70     70      40             60              40
    ## 24     shiftry        275       grass/dark   chlorophyll  90    100      60             90              60
    ## 25     taillow        276    normal/flying          guts  40     55      30             30              30
    ## 26     swellow        277    normal/flying          guts  60     85      60             75              50
    ## 27     wingull        278     water/flying      keen-eye  40     30      30             55              30
    ## 28    pelipper        279     water/flying      keen-eye  60     50     100             95              70
    ## 29       ralts        280    psychic/fairy   synchronize  28     25      25             45              35
    ## 30      kirlia        281    psychic/fairy   synchronize  38     35      35             65              55
    ## 31   gardevoir        282    psychic/fairy   synchronize  68     65      65            125             115
    ## 32     surskit        283        bug/water    swift-swim  40     30      32             50              52
    ## 33  masquerain        284       bug/flying    intimidate  70     60      62            100              82
    ## 34   shroomish        285            grass  effect-spore  60     40      60             40              60
    ## 35     breloom        286   grass/fighting  effect-spore  60    130      80             60              60
    ## 36     slakoth        287           normal        truant  60     60      60             35              35
    ## 37    vigoroth        288           normal  vital-spirit  80     80      80             55              55
    ## 38     slaking        289           normal        truant 150    160     100             95              65
    ## 39     nincada        290       bug/ground compound-eyes  31     45      90             30              30
    ## 40     ninjask        291       bug/flying   speed-boost  61     90      45             50              50
    ## 41    shedinja        292        bug/ghost  wonder-guard   1     90      45             30              30
    ## 42     whismur        293           normal    soundproof  64     51      23             51              23
    ## 43     loudred        294           normal    soundproof  84     71      43             71              43
    ## 44     exploud        295           normal    soundproof 104     91      63             91              73
    ## 45    makuhita        296         fighting     thick-fat  72     60      30             20              30
    ## 46    hariyama        297         fighting     thick-fat 144    120      60             40              60
    ## 47     azurill        298     normal/fairy     thick-fat  50     20      40             20              40
    ## 48    nosepass        299             rock        sturdy  30     45     135             45              90
    ## 49      skitty        300           normal    cute-charm  50     45      45             35              35
    ## 50    delcatty        301           normal    cute-charm  70     65      65             55              55
    ## 51     sableye        302       dark/ghost      keen-eye  50     75      75             65              65
    ## 52      mawile        303      steel/fairy  hyper-cutter  50     85      85             55              55
    ## 53        aron        304       steel/rock        sturdy  50     70     100             40              40
    ## 54      lairon        305       steel/rock        sturdy  60     90     140             50              50
    ## 55      aggron        306       steel/rock        sturdy  70    110     180             60              60
    ## 56    meditite        307 fighting/psychic    pure-power  30     40      55             40              55
    ## 57    medicham        308 fighting/psychic    pure-power  60     60      75             60              75
    ## 58   electrike        309         electric        static  40     45      40             65              40
    ## 59   manectric        310         electric        static  70     75      60            105              60
    ## 60      plusle        311         electric          plus  60     50      40             85              75
    ## 61       minun        312         electric         minus  60     40      50             75              85
    ## 62     volbeat        313              bug    illuminate  65     73      75             47              85
    ## 63    illumise        314              bug     oblivious  65     47      75             73              85
    ## 64     roselia        315     grass/poison  natural-cure  50     60      45            100              80
    ## 65      gulpin        316           poison   liquid-ooze  70     43      53             43              53
    ## 66      swalot        317           poison   liquid-ooze 100     73      83             73              83
    ## 67    carvanha        318       water/dark    rough-skin  45     90      20             65              20
    ## 68    sharpedo        319       water/dark    rough-skin  70    120      40             95              40
    ## 69     wailmer        320            water    water-veil 130     70      35             70              35
    ## 70     wailord        321            water    water-veil 170     90      45             90              45
    ## 71       numel        322      fire/ground     oblivious  60     60      40             65              45
    ## 72    camerupt        323      fire/ground   magma-armor  70    100      70            105              75
    ## 73     torkoal        324             fire   white-smoke  70     85     140             85              70
    ## 74      spoink        325          psychic     thick-fat  60     25      35             70              80
    ## 75     grumpig        326          psychic     thick-fat  80     45      65             90             110
    ## 76      spinda        327           normal     own-tempo  60     60      60             60              60
    ## 77    trapinch        328           ground  hyper-cutter  45    100      45             45              45
    ## 78     vibrava        329    ground/dragon      levitate  50     70      50             50              50
    ## 79      flygon        330    ground/dragon      levitate  80    100      80             80              80
    ## 80      cacnea        331            grass     sand-veil  50     85      40             85              40
    ## 81    cacturne        332       grass/dark     sand-veil  70    115      60            115              60
    ## 82      swablu        333    normal/flying  natural-cure  45     40      60             40              75
    ## 83     altaria        334    dragon/flying  natural-cure  75     70      90             70             105
    ## 84    zangoose        335           normal      immunity  73    115      60             60              60
    ## 85     seviper        336           poison     shed-skin  73    100      60            100              60
    ## 86    lunatone        337     rock/psychic      levitate  90     55      65             95              85
    ## 87     solrock        338     rock/psychic      levitate  90     95      85             55              65
    ## 88    barboach        339     water/ground     oblivious  50     48      43             46              41
    ## 89    whiscash        340     water/ground     oblivious 110     78      73             76              71
    ## 90    corphish        341            water  hyper-cutter  43     80      65             50              35
    ## 91   crawdaunt        342       water/dark  hyper-cutter  63    120      85             90              55
    ## 92      baltoy        343   ground/psychic      levitate  40     40      55             40              70
    ## 93     claydol        344   ground/psychic      levitate  60     70     105             70             120
    ## 94      lileep        345       rock/grass  suction-cups  66     41      77             61              87
    ## 95     cradily        346       rock/grass  suction-cups  86     81      97             81             107
    ## 96     anorith        347         rock/bug  battle-armor  45     95      50             40              50
    ## 97     armaldo        348         rock/bug  battle-armor  75    125     100             70              80
    ## 98      feebas        349            water    swift-swim  20     15      20             10              55
    ## 99     milotic        350            water  marvel-scale  95     60      79            100             125
    ## 100   castform        351           normal      forecast  70     70      70             70              70
    ##     speed
    ## 1      70
    ## 2      95
    ## 3     120
    ## 4      45
    ## 5      55
    ## 6      80
    ## 7      40
    ## 8      50
    ## 9      60
    ## 10     35
    ## 11     70
    ## 12     60
    ## 13    100
    ## 14     20
    ## 15     15
    ## 16     65
    ## 17     15
    ## 18     65
    ## 19     30
    ## 20     50
    ## 21     70
    ## 22     30
    ## 23     60
    ## 24     80
    ## 25     85
    ## 26    125
    ## 27     85
    ## 28     65
    ## 29     40
    ## 30     50
    ## 31     80
    ## 32     65
    ## 33     80
    ## 34     35
    ## 35     70
    ## 36     30
    ## 37     90
    ## 38    100
    ## 39     40
    ## 40    160
    ## 41     40
    ## 42     28
    ## 43     48
    ## 44     68
    ## 45     25
    ## 46     50
    ## 47     20
    ## 48     30
    ## 49     50
    ## 50     90
    ## 51     50
    ## 52     50
    ## 53     30
    ## 54     40
    ## 55     50
    ## 56     60
    ## 57     80
    ## 58     65
    ## 59    105
    ## 60     95
    ## 61     95
    ## 62     85
    ## 63     85
    ## 64     65
    ## 65     40
    ## 66     55
    ## 67     65
    ## 68     95
    ## 69     60
    ## 70     60
    ## 71     35
    ## 72     40
    ## 73     20
    ## 74     60
    ## 75     80
    ## 76     60
    ## 77     10
    ## 78     70
    ## 79    100
    ## 80     35
    ## 81     55
    ## 82     50
    ## 83     80
    ## 84     90
    ## 85     65
    ## 86     70
    ## 87     70
    ## 88     60
    ## 89     60
    ## 90     35
    ## 91     55
    ## 92     55
    ## 93     75
    ## 94     23
    ## 95     43
    ## 96     75
    ## 97     45
    ## 98     80
    ## 99     81
    ## 100    70
    ##  [ reached 'max' / getOption("max.print") -- omitted 35 rows ]

### `Game Version`

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
typedex(11)
```

    ##                      name move_id accuracy power pp crit_rate drain flinch_chance
    ## 1               water-gun      55      100    40 25         0     0             0
    ## 2              hydro-pump      56       80   110  5         0     0             0
    ## 3                    surf      57      100    90 15         0     0             0
    ## 4             bubble-beam      61      100    65 20         0     0             0
    ## 5                withdraw     110       NA    NA 40         0     0             0
    ## 6               waterfall     127      100    80 15         0     0            20
    ## 7                   clamp     128       85    35 15         0     0             0
    ## 8                  bubble     145      100    40 30         0     0             0
    ## 9              crabhammer     152       90   100 10         1     0             0
    ## 10              octazooka     190       85    65 10         0     0             0
    ## 11             rain-dance     240       NA    NA  5         0     0             0
    ## 12              whirlpool     250       85    35 15         0     0             0
    ## 13                   dive     291      100    80 10         0     0             0
    ## 14           hydro-cannon     308       90   150  5         0     0             0
    ## 15            water-spout     323      100   150  5         0     0             0
    ## 16            muddy-water     330       85    90 10         0     0             0
    ## 17            water-sport     346       NA    NA 15         0     0             0
    ## 18            water-pulse     352      100    60 20         0     0             0
    ## 19                  brine     362      100    65 10         0     0             0
    ## 20              aqua-ring     392       NA    NA 20         0     0             0
    ## 21              aqua-tail     401       90    90 10         0     0             0
    ## 22               aqua-jet     453      100    40 20         0     0             0
    ## 23                   soak     487      100    NA 20         0     0             0
    ## 24                  scald     503      100    80 15         0     0             0
    ## 25           water-pledge     518      100    80 10         0     0             0
    ## 26            razor-shell     534       95    75 10         0     0             0
    ## 27         steam-eruption     592       95   110  5         0     0             0
    ## 28         water-shuriken     594      100    15 20         0     0             0
    ## 29           origin-pulse     618       85   110 10         0     0             0
    ## 30 hydro-vortex--physical     642       NA    NA  1         0     0             0
    ## 31  hydro-vortex--special     643       NA    NA  1         0     0             0
    ## 32         sparkling-aria     664      100    90 10         0     0             0
    ## 33       oceanic-operetta     697       NA   195  1         0     0             0
    ## 34            liquidation     710      100    85 10         0     0             0
    ## 35         splishy-splash     730      100    90 15         0     0             0
    ## 36          bouncy-bubble     733      100    60 20         0   100             0
    ## 37             snipe-shot     745      100    80 15         1     0             0
    ## 38          fishious-rend     755      100    85 10         0     0             0
    ## 39             max-geyser     765       NA    10 10         0     0             0
    ## 40               life-dew     791       NA    NA 10         0     0             0
    ## 41              flip-turn     812      100    60 20         0     0             0
    ## 42        surging-strikes     818      100    25  5         6     0             0

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
    output <- franchise(...)
  }
  else if (func == "gendex"){
    output <- teamTotals(...)
  }
  else if (func == "typedex"){
    output <- seasonRecords(...)
  }
  else {
    stop("ERROR: Argument for func is not valid!")
  }
  
  # Return the output from the appropriate function.
  return(output)
}
```
