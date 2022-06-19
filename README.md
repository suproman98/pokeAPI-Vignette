Becoming the Very Best API: Contacting the pokeAPI
================
Supro Debnath
2022-06-18

-   [Requirements](#requirements)
-   [pokeAPI Functions](#pokeapi-functions)
    -   [`Game Version`](#game-version)
    -   [`Generation Pokedex`](#generation-pokedex)
    -   [`Wrapper Function`](#wrapper-function)

This vignette is a step-by-step guide to interacting with the
[pokeAPI](https://pokeapi.co/docs/v2#info). There are a few functions in
here that explore the data present, as well as some visualizations that
highlight interesting patterns in the world of pokemon. Most of the ids
are represented as numeric (i.e. `Generation 1` referring to the Kanto
pokemon, or `Version 8` referring to the Hoenn region). For that reason,
if you intend on using any of these functions, please be mindful of how
the function inputs correspond to the data you are trying to pull.

## Requirements

To interact with this API, a few packages must be installed. +
`jsonlite`: Works with JSON in R and is useful for parsing data and
interacting with a web API. + `httr`: Provides useful tools for working
with HTTP. + `tidyverse`: Collection of R packages that contribute
heavily to API interaction and data visualization. + `knitr`: Provides
tools for dynamic reporting in R. + `dplyr`: Package that provides tools
for working with data frames. + `ggplot2`: Powerful for complex data
visualizations.

``` r
library(jsonlite)
library(httr)
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ───────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ purrr::flatten() masks jsonlite::flatten()
    ## ✖ dplyr::lag()     masks stats::lag()

``` r
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
abilities), and their `stats` (attack, speed, etc.). It takes a numeric
input from `1` to `8`, which is the order of each generation being
introduced.

``` r
gendex <- function(gen) {
  if ((gen > 8) || (gen < 1)) {
    stop("This is a non-existent generation. Please refine search or submit a numeric input.")
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
gendex(2)
```

    ##           name pokedex_id           type      ability  hp attack defense special.attack special.defense
    ## 1    chikorita        152          grass     overgrow  45     49      65             49              65
    ## 2      bayleef        153          grass     overgrow  60     62      80             63              80
    ## 3     meganium        154          grass     overgrow  80     82     100             83             100
    ## 4    cyndaquil        155           fire        blaze  39     52      43             60              50
    ## 5      quilava        156           fire        blaze  58     64      58             80              65
    ## 6   typhlosion        157           fire        blaze  78     84      78            109              85
    ## 7     totodile        158          water      torrent  50     65      64             44              48
    ## 8     croconaw        159          water      torrent  65     80      80             59              63
    ## 9   feraligatr        160          water      torrent  85    105     100             79              83
    ## 10     sentret        161         normal     run-away  35     46      34             35              45
    ## 11      furret        162         normal     run-away  85     76      64             45              55
    ## 12    hoothoot        163  normal/flying     insomnia  60     30      30             36              56
    ## 13     noctowl        164  normal/flying     insomnia 100     50      50             86              96
    ## 14      ledyba        165     bug/flying        swarm  40     20      30             40              80
    ## 15      ledian        166     bug/flying        swarm  55     35      50             55             110
    ## 16    spinarak        167     bug/poison        swarm  40     60      40             40              40
    ## 17     ariados        168     bug/poison        swarm  70     90      70             60              70
    ## 18      crobat        169  poison/flying  inner-focus  85     90      80             70              80
    ## 19    chinchou        170 water/electric  volt-absorb  75     38      38             56              56
    ## 20     lanturn        171 water/electric  volt-absorb 125     58      58             76              76
    ## 21       pichu        172       electric       static  20     40      15             35              35
    ## 22      cleffa        173          fairy   cute-charm  50     25      28             45              55
    ## 23   igglybuff        174   normal/fairy   cute-charm  90     30      15             40              20
    ## 24      togepi        175          fairy       hustle  35     20      65             40              65
    ## 25     togetic        176   fairy/flying       hustle  55     40      85             80             105
    ## 26        natu        177 psychic/flying  synchronize  40     50      45             70              45
    ## 27        xatu        178 psychic/flying  synchronize  65     75      70             95              70
    ## 28      mareep        179       electric       static  55     40      40             65              45
    ## 29     flaaffy        180       electric       static  70     55      55             80              60
    ## 30    ampharos        181       electric       static  90     75      85            115              90
    ## 31   bellossom        182          grass  chlorophyll  75     80      95             90             100
    ## 32      marill        183    water/fairy    thick-fat  70     20      50             20              50
    ## 33   azumarill        184    water/fairy    thick-fat 100     50      80             60              80
    ## 34   sudowoodo        185           rock       sturdy  70    100     115             30              65
    ## 35    politoed        186          water water-absorb  90     75      75             90             100
    ## 36      hoppip        187   grass/flying  chlorophyll  35     35      40             35              55
    ## 37    skiploom        188   grass/flying  chlorophyll  55     45      50             45              65
    ## 38    jumpluff        189   grass/flying  chlorophyll  75     55      70             55              95
    ## 39       aipom        190         normal     run-away  55     70      55             40              55
    ## 40     sunkern        191          grass  chlorophyll  30     30      30             30              30
    ## 41    sunflora        192          grass  chlorophyll  75     75      55            105              85
    ## 42       yanma        193     bug/flying  speed-boost  65     65      45             75              45
    ## 43      wooper        194   water/ground         damp  55     45      45             25              25
    ## 44    quagsire        195   water/ground         damp  95     85      85             65              65
    ## 45      espeon        196        psychic  synchronize  65     65      60            130              95
    ## 46     umbreon        197           dark  synchronize  95     65     110             60             130
    ## 47     murkrow        198    dark/flying     insomnia  60     85      42             85              42
    ## 48    slowking        199  water/psychic    oblivious  95     75      80            100             110
    ## 49  misdreavus        200          ghost     levitate  60     60      60             85              85
    ## 50       unown        201        psychic     levitate  48     72      48             72              48
    ## 51   wobbuffet        202        psychic   shadow-tag 190     33      58             33              58
    ## 52   girafarig        203 normal/psychic  inner-focus  70     80      65             90              65
    ## 53      pineco        204            bug       sturdy  50     65      90             35              35
    ## 54  forretress        205      bug/steel       sturdy  75     90     140             60              60
    ## 55   dunsparce        206         normal serene-grace 100     70      70             65              65
    ## 56      gligar        207  ground/flying hyper-cutter  65     75     105             35              65
    ## 57     steelix        208   steel/ground    rock-head  75     85     200             55              65
    ## 58    snubbull        209          fairy   intimidate  60     80      50             40              40
    ## 59    granbull        210          fairy   intimidate  90    120      75             60              60
    ## 60    qwilfish        211   water/poison poison-point  65     95      85             55              55
    ## 61      scizor        212      bug/steel        swarm  70    130     100             55              80
    ## 62     shuckle        213       bug/rock       sturdy  20     10     230             10             230
    ## 63   heracross        214   bug/fighting        swarm  80    125      75             40              95
    ## 64     sneasel        215       dark/ice  inner-focus  55     95      55             35              75
    ## 65   teddiursa        216         normal       pickup  60     80      50             50              50
    ## 66    ursaring        217         normal         guts  90    130      75             75              75
    ## 67      slugma        218           fire  magma-armor  40     40      40             70              40
    ## 68    magcargo        219      fire/rock  magma-armor  60     50     120             90              80
    ## 69      swinub        220     ice/ground    oblivious  50     50      40             30              30
    ## 70   piloswine        221     ice/ground    oblivious 100    100      80             60              60
    ## 71     corsola        222     water/rock       hustle  65     55      95             65              95
    ## 72    remoraid        223          water       hustle  35     65      35             65              35
    ## 73   octillery        224          water suction-cups  75    105      75            105              75
    ## 74    delibird        225     ice/flying vital-spirit  45     55      45             65              45
    ## 75     mantine        226   water/flying   swift-swim  85     40      70             80             140
    ## 76    skarmory        227   steel/flying     keen-eye  65     80     140             40              70
    ## 77    houndour        228      dark/fire   early-bird  45     60      30             80              50
    ## 78    houndoom        229      dark/fire   early-bird  75     90      50            110              80
    ## 79     kingdra        230   water/dragon   swift-swim  75     95      95             95              95
    ## 80      phanpy        231         ground       pickup  90     60      60             40              40
    ## 81     donphan        232         ground       sturdy  90    120     120             60              60
    ## 82    porygon2        233         normal        trace  85     80      90            105              95
    ## 83    stantler        234         normal   intimidate  73     95      62             85              65
    ## 84    smeargle        235         normal    own-tempo  55     20      35             20              45
    ## 85     tyrogue        236       fighting         guts  35     35      35             35              35
    ## 86   hitmontop        237       fighting   intimidate  50     95      95             35             110
    ## 87    smoochum        238    ice/psychic    oblivious  45     30      15             85              65
    ## 88      elekid        239       electric       static  45     63      37             65              55
    ## 89       magby        240           fire   flame-body  45     75      37             70              55
    ## 90     miltank        241         normal    thick-fat  95     80     105             40              70
    ## 91     blissey        242         normal natural-cure 255     10      10             75             135
    ## 92      raikou        243       electric     pressure  90     85      75            115             100
    ## 93       entei        244           fire     pressure 115    115      85             90              75
    ## 94     suicune        245          water     pressure 100     75     115             90             115
    ## 95    larvitar        246    rock/ground         guts  50     64      50             45              50
    ## 96     pupitar        247    rock/ground    shed-skin  70     84      70             65              70
    ## 97   tyranitar        248      rock/dark  sand-stream 100    134     110             95             100
    ## 98       lugia        249 psychic/flying     pressure 106     90     130             90             154
    ## 99       ho-oh        250    fire/flying     pressure 106    130      90            110             154
    ## 100     celebi        251  psychic/grass natural-cure 100    100     100            100             100
    ##     speed
    ## 1      45
    ## 2      60
    ## 3      80
    ## 4      65
    ## 5      80
    ## 6     100
    ## 7      43
    ## 8      58
    ## 9      78
    ## 10     20
    ## 11     90
    ## 12     50
    ## 13     70
    ## 14     55
    ## 15     85
    ## 16     30
    ## 17     40
    ## 18    130
    ## 19     67
    ## 20     67
    ## 21     60
    ## 22     15
    ## 23     15
    ## 24     20
    ## 25     40
    ## 26     70
    ## 27     95
    ## 28     35
    ## 29     45
    ## 30     55
    ## 31     50
    ## 32     40
    ## 33     50
    ## 34     30
    ## 35     70
    ## 36     50
    ## 37     80
    ## 38    110
    ## 39     85
    ## 40     30
    ## 41     30
    ## 42     95
    ## 43     15
    ## 44     35
    ## 45    110
    ## 46     65
    ## 47     91
    ## 48     30
    ## 49     85
    ## 50     48
    ## 51     33
    ## 52     85
    ## 53     15
    ## 54     40
    ## 55     45
    ## 56     85
    ## 57     30
    ## 58     30
    ## 59     45
    ## 60     85
    ## 61     65
    ## 62      5
    ## 63     85
    ## 64    115
    ## 65     40
    ## 66     55
    ## 67     20
    ## 68     30
    ## 69     50
    ## 70     50
    ## 71     35
    ## 72     65
    ## 73     45
    ## 74     75
    ## 75     70
    ## 76     70
    ## 77     65
    ## 78     95
    ## 79     85
    ## 80     40
    ## 81     50
    ## 82     60
    ## 83     85
    ## 84     75
    ## 85     35
    ## 86     70
    ## 87     65
    ## 88     95
    ## 89     83
    ## 90    100
    ## 91     55
    ## 92    115
    ## 93    100
    ## 94     85
    ## 95     41
    ## 96     51
    ## 97     61
    ## 98    110
    ## 99     90
    ## 100   100

### `Wrapper Function`

``` r
pokeAPI <- function(func, ...) {
  ###
  # This function is a wrapper for the other functions. It takes in the name
  # of the function to use as a character and any additional arguments for that
  # function.
  ###
  
  # Find and call the appropriate function using conditional logic.
  
  if (func == "version") {
    output <- version(...)
  }
  else if (func == "gendex") {
    output <- gendex(...)
  }
  else {
    stop("ERROR: Argument for func is not valid!")
  }
  # Return the output from the appropriate function.
  return(output)
}
```
