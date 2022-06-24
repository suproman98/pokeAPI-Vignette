Becoming the Very Best API: Contacting the pokeAPI
================
Supro Debnath
2022-06-18

-   [Requirements](#requirements)
-   [pokeAPI Functions](#pokeapi-functions)
    -   [`Generation Pokedex`](#generation-pokedex)
    -   [`TypeDex`](#typedex)
    -   [`pokeAPI Wrapper Function`](#pokeapi-wrapper-function)
-   [Visuals](#visuals)
    -   [`The Original 151: Kanto Region`](#the-original-151-kanto-region)
    -   [`The Childhood Favorite: Hoenn Region`](#the-childhood-favorite-hoenn-region)
    -   [`The People's Champ: Sinnoh Region`](#the-peoples-champ-sinnoh-region)

This vignette is a step-by-step guide to interacting with the
[pokeAPI](https://pokeapi.co/docs/v2). There are a few functions in here
that explore the data present, as well as some visualizations that
highlight interesting patterns in the world of pokemon. Most of the ids
are represented as numeric (i.e. `Generation 1` referring to the Kanto
pokemon, or `Type 3` referring to the `Flying` type). For that reason,
if you intend on using any of these functions, please be mindful of how
the function inputs correspond to the data you are trying to pull.

## Requirements

To interact with this API, a few packages must be installed. 1.
jsonlite: Works with JSON in R and is useful for parsing data and
interacting with a web API. 2. httr: Provides useful tools for working
with HTTP. 3. tidyverse: Collection of R packages that contribute
heavily to API interaction and data visualization. 4. knitr: Provides
tools for dynamic reporting in R. 5. dplyr: Package that provides tools
for working with data frames. 6. ggplot2: Powerful for complex data
visualizations.

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

This section is dedicated to the two functions I’ve created to pull data
from this API. This API had a lot of information I found irrelevant for
my analysis, as I want to hone in on which pokemon are the best and
which type is the most prevalent.

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
  
  #Create a conditional input statement for this function. If an invalid input is put in, the function breaks.
  if ((gen > 8) || (gen < 1)) {
    stop("This is a non-existent generation. Please refine search or submit a numeric input from 1 to 8.")
  }
  
  #Parse through the data from the given URL.
  respGEN <- GET(paste0("http://pokeapi.co/api/v2/generation/", gen))
  conGEN <- content(respGEN, "parsed")
  
  #Create an empty data frame of the moves URL that will provide stats on different moves.
  df_pokeGEN <- do.call(rbind.data.frame, c(conGEN$pokemon_species, stringsAsFactors = FALSE))
  
  #Create a function that will parse through the URLs in the move page.
  pokePULL <- function(url) {
    specCON <- content(GET(url), "parsed")
  
  #Call in the variable capture_rate from the pokemon_species URL.
    catch_rate <- specCON$capture_rate
    
  #Parse through the varieties URL and get relevant information.
    pokeURL <- specCON$varieties[[1]]$pokemon$url
    pokeCON <- content(GET(pokeURL), "parsed")
    
  #Filter through all relevant statistics for each pokemon and label them accordingly.
    stats <- sapply(pokeCON$stats, function(stats) stats[["base_stat"]])
    names(stats) <- sapply(pokeCON$stats, function(stats) stats[["stat"]][["name"]])
    
  #Create a base loop for going through typing. There are 2 slots, indicating a combo type or single type.
    for (i in seq_along(pokeCON$types)) {
      if (pokeCON$types[[i]][["slot"]] == 1){
        type <- pokeCON$types[[1]][["type"]][["name"]]
      }
      else {
        type <- paste0(pokeCON$types[[1]][["type"]][["name"]], "/",      
                       pokeCON$types[[2]][["type"]][["name"]])
      }
    }
    
  #Same loop structure for abilities, although we are only going to look at main abilities. To analyze hidden or secondary abilities, add more options.
    for (i in seq_along(pokeCON$abilities)) {
      if (pokeCON$abilities[[i]][["slot"]] == 1) {
        ability <- pokeCON$abilities[[1]][["ability"]][["name"]]
      } 
    }
 
  #Return necessary Pokemon information.
    return(
      c(
        name = pokeCON$name,
        pokedex_id = pokeCON$id,
        type = type,
        ability = ability,
        weight = pokeCON$weight,
        as.list(stats),
        catch_rate = round((catch_rate/255)*100)
      )
    )
  }
  
  #Clean up data frame and organize data by Pokemon ID.
  pokeLIST <- lapply(df_pokeGEN$url, pokePULL)
  pokedex <- do.call(rbind.data.frame, c(pokeLIST, stringsAsFactors = FALSE))
  pokedex <- pokedex %>% arrange(pokedex_id)
  colnames(pokedex)[12] <- "catch_rate_pct"
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
  
  #Create a conditional input statement for this function. If an invalid input is put in, the function breaks.
  if ((type > 20) || (type < 1)) {
    stop("This is a non-existent typing. Please refine search or submit a numeric input from 1 to 20.")
  }
  
  #Parse through the data from the given URL.
  respTYPE <- GET(paste0("http://pokeapi.co/api/v2/type/", type))
  conTYPE <- content(respTYPE, "parsed")
  
  #Create an empty data frame of the moves URL that will provide stats on different moves.
  df_typeGEN <- do.call(rbind.data.frame, c(conTYPE$moves, stringsAsFactors = FALSE))

  #Create a function that will parse through the URLs in the move page.
  typePULL <- function(url) {
    moveCON <- content(GET(url), "parsed")
    
    #IF statements that filter out values listed as null for specific categories.
    if (is.null(moveCON$accuracy)) {
        moveCON$accuracy = "NA"
    }
    
    if (is.null(moveCON$power)) {
        moveCON$power = "NA"
    }
    
    #Return necessary stats.
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
  
  #Clean up data frame and insert new names for relevant columns.
  typeLIST <- lapply(df_typeGEN$url, typePULL)
  typedex <- do.call(rbind.data.frame, c(typeLIST, stringsAsFactors = FALSE))
  names(typedex) <- c('name', 'move_id', 'accuracy', 'power', 'pp', 'crit_rate', 'drain', 'flinch_chance')
  typedex <- type.convert(typedex)
  return(typedex)
}
```

### `pokeAPI Wrapper Function`

Wrapper function for all the functions created.

``` r
pokeAPI <- function(func, ...){
  ###
  # This function is a wrapper for the other functions. It takes in the name
  # of the function to use as a character and any additional arguments for that
  # function.
  ###
  
  # Find and call the appropriate function using conditional logic.
  
  if (func == "gen"){
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

## Visuals

### `The Original 151: Kanto Region`

To start the data exploration, I wanted to look at the generation where
the phrase “Gotta Catch Em All” came from. What pokemon/pokemon types
are the best in the game? Is there really a reason to picking Charizard
over Blastoise, and is Venusaur really that useless? To start, we will
load in the Pokedex information with our `pokeAPI` wrapper function,
then add in a new variable called `totalstats` that is the aggregate of
all six pokemon stats.

``` r
kanto <- pokeAPI("gen", 1)
kanto <- kanto %>% mutate(totalstats = hp+attack+defense+special.attack+special.defense+speed)
head(kanto)
```

    ##         name pokedex_id         type  ability weight hp attack defense special.attack special.defense speed
    ## 1  bulbasaur          1 grass/poison overgrow     69 45     49      49             65              65    45
    ## 2    ivysaur          2 grass/poison overgrow    130 60     62      63             80              80    60
    ## 3   venusaur          3 grass/poison overgrow   1000 80     82      83            100             100    80
    ## 4 charmander          4         fire    blaze     85 39     52      43             60              50    65
    ## 5 charmeleon          5         fire    blaze    190 58     64      58             80              65    80
    ## 6  charizard          6  fire/flying    blaze    905 78     84      78            109              85   100
    ##   catch_rate_pct totalstats
    ## 1             18        318
    ## 2             18        405
    ## 3             18        525
    ## 4             18        309
    ## 5             18        405
    ## 6             18        534

With our data loaded, I now want to look at the top 25 strongest pokemon
from this generation by total stats. While I anticipate most of the
legendary pokemon being included in this list, I do want to see what
types are the most prevalent (or combination of types).

``` r
kanto_strongest <- top_n(kanto, n=25, totalstats)

plot1 <- ggplot(kanto_strongest, aes(x=name, y=totalstats, fill=type)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Strongest Kanto Pokemon") + xlab("Pokemon Name") + ylab("Total Stats")  + theme(plot.title = element_text(hjust = 0.5))
plot1
```

![](README_files/figure-gfm/kanto%20strongest-1.png)<!-- -->

Let’s dive into the chances of catching these pokemon in the wild (not
taking into account pokeball type).

``` r
plot2 <- ggplot(kanto_strongest, aes(x=name, y=catch_rate_pct, fill=type)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Strongest Kanto Pokemon Catch Rate") + xlab("Pokemon Name") + ylab("Catch Rate %")  + theme(plot.title = element_text(hjust = 0.5))
plot2
```

![](README_files/figure-gfm/kanto%20catch%20rate-1.png)<!-- -->

All three starters are present, though Charizard has a slight edge.
After looking at the typing and the Pokemon included, I wanted to see
the spread of total stats, seeing whether the most common Pokemon total
stat number was.

``` r
plot3 <- ggplot(kanto, aes(x=totalstats)) + geom_histogram() + ggtitle("Total Stat Spread") + xlab("Total Stats") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5))
plot3
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/kanto%20total%20stat-1.png)<!-- -->

Next, we want to look at the typing breakdown and the count of each in
the entire set of 151 pokemon.

``` r
plot4 <- ggplot(kanto, aes(x=type, fill=type))  + geom_bar() + coord_flip() + ggtitle("Kanto Type Combinations") + xlab("Types") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5))
plot4
```

![](README_files/figure-gfm/kanto%20type%20count-1.png)<!-- -->

There are an absurd number of water-type pokemon, with a few other types
having high counts compared to the rest. I want to see the spread of
`totalstat` by type and see which types are better overall for stats.

``` r
kanto_spread <- filter(kanto, type == 'water' | type == 'normal' | type == 'poison' | type == 'fire' | type == 'grass/poison')

plot5 <- ggplot(kanto_spread, aes(x=type, y=totalstats, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16) + coord_flip() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + ggtitle("Total Stat Spread of Most Prevalent Types") + xlab("Total Stats") + ylab("Pokemon Type") 
plot5
```

![](README_files/figure-gfm/specific%20kanto%20types-1.png)<!-- -->

It looks like on average, fire type pokemon are overall stronger and
will have higher overall stats than the rest of the types. Now, lets
look at a table of all the fire type moves that are available for these
pokemon to learn.

``` r
k_moves <- pokeAPI("type", 10)
table1 <- table(k_moves$power, k_moves$pp)
knitr::kable(table1, format = "pipe", caption = "Power of Fire Moves by PP")
```

|     |   1 |   5 |  10 |  15 |  20 |  25 |
|:----|----:|----:|----:|----:|----:|----:|
| 35  |   0 |   0 |   0 |   1 |   0 |   0 |
| 40  |   0 |   0 |   0 |   0 |   0 |   1 |
| 50  |   0 |   0 |   0 |   0 |   1 |   0 |
| 60  |   0 |   0 |   0 |   1 |   1 |   1 |
| 65  |   0 |   0 |   0 |   1 |   0 |   0 |
| 70  |   0 |   1 |   0 |   1 |   0 |   0 |
| 75  |   0 |   0 |   1 |   1 |   0 |   0 |
| 80  |   0 |   0 |   2 |   2 |   0 |   0 |
| 85  |   0 |   0 |   1 |   0 |   0 |   0 |
| 90  |   0 |   0 |   0 |   1 |   0 |   0 |
| 95  |   0 |   0 |   1 |   0 |   0 |   0 |
| 100 |   0 |   5 |   1 |   0 |   0 |   0 |
| 110 |   0 |   1 |   0 |   0 |   0 |   0 |
| 120 |   0 |   1 |   0 |   1 |   0 |   0 |
| 130 |   0 |   3 |   0 |   0 |   0 |   0 |
| 150 |   0 |   4 |   0 |   0 |   0 |   0 |
| 180 |   0 |   1 |   0 |   0 |   0 |   0 |

Power of Fire Moves by PP

There are a wide assortment of fire moves with power over 100. With the
total stats being high, as well as the move strength being high on
average as well, lets take a look at some individual stats.

“A strong defense is the best offense” is most commonly stated, and in
this case there is somewhat of a relationship between the two.

``` r
plot6 <- ggplot(kanto, aes(x=attack, y=defense)) + geom_point(stat='identity') + geom_smooth() + stat_cor(mapping = NULL, data = NULL, method = "pearson", alternative = "two.sided") + ggtitle("Attack v Defense Correlation") + xlab("Attack") + ylab("Defense") + theme(plot.title = element_text(hjust = 0.5))
plot6
```

![](README_files/figure-gfm/attack%20v%20defense%20kanto-1.png)<!-- -->

However, in the realm of Pokemon (especially with competitive battling),
the most utilized stat is speed (Gotta Catch Em All or Gotta Go Fast?).
In any case, we want to see the types with the most speed and see how it
corresponds to the results we have already found.

``` r
plot7 <- ggplot(kanto_spread, aes(x=type, y=speed, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16) + coord_flip() + theme(legend.position = "none") + ggtitle("Kanto Speed of Most Prevalent Types") + xlab("Types") + ylab("Speed Stat") + theme(plot.title = element_text(hjust = 0.5)) 
plot7
```

![](README_files/figure-gfm/speed%20kanto-1.png)<!-- -->

Fire seems to be the predominantly powerful type in the original list of
151. WIth pokemon like `Moltres`, `Charizard`, `Flareon` and `Arcanine`,
there are a good assortment of options to choose from, as well as
teaching powerful moves and using them repeatedly to win battles.
However, we know that this generation isn’t the only one present! Let’s
take a deeper dive into one of my personal favorites: Generation 3.

### `The Childhood Favorite: Hoenn Region`

We will be following the same structure as the Kanto. There are a
completely new set of pokemon to explore, and we want to answer the same
question: Which type of pokemon is the best?

``` r
hoenn <- pokeAPI("gen", 3)
hoenn <- hoenn %>% mutate(totalstats = hp+attack+defense+special.attack+special.defense+speed)
head(hoenn)
```

    ##        name pokedex_id          type  ability weight hp attack defense special.attack special.defense speed
    ## 1   treecko        252         grass overgrow     50 40     45      35             65              55    70
    ## 2   grovyle        253         grass overgrow    216 50     65      45             85              65    95
    ## 3  sceptile        254         grass overgrow    522 70     85      65            105              85   120
    ## 4   torchic        255          fire    blaze     25 45     60      40             70              50    45
    ## 5 combusken        256 fire/fighting    blaze    195 60     85      60             85              60    55
    ## 6  blaziken        257 fire/fighting    blaze    520 80    120      70            110              70    80
    ##   catch_rate_pct totalstats
    ## 1             18        310
    ## 2             18        405
    ## 3             18        530
    ## 4             18        310
    ## 5             18        405
    ## 6             18        530

Let’s look at the strongest pokemon. As always, legendaries will be a
good portion of the list, but let’s see if there are any surprising
additions.

``` r
hoenn_strongest <- top_n(hoenn, n=25, totalstats)

plot8 <- ggplot(hoenn_strongest, aes(x=name, y=totalstats, fill=type)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Strongest Hoenn Pokemon") + xlab("Pokemon Name") + ylab("Total Stats") + theme(plot.title = element_text(hjust = 0.5))
plot8
```

![](README_files/figure-gfm/hoenn%20strongest-1.png)<!-- -->

The three starters are present (Blaziken, Sceptile, and Swampert), with
Rayquaza being the strongest legendary and pokemon overall. There are a
lot higher total stat numbers with this generation than in Kanto, but
the craziest find was that Slaking, a very common pokemon, has the
second highest total stat count. We’ll dive in further by looking at the
`catch rate`.

``` r
plot9 <- ggplot(hoenn_strongest, aes(x=name, y=catch_rate_pct, fill=type)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Strongest Hoenn Pokemon Catch Rate") + xlab("Pokemon Name") + ylab("Catch Rate %") + theme(plot.title = element_text(hjust = 0.5))
plot9
```

![](README_files/figure-gfm/hoenn%20catch%20rate-1.png)<!-- -->

`Slaking` actually has a relatively decent `catch rate`, while pokemon
like `Milotic`, `Claydol`, and `Wailord` are pretty high in `catch rate`
compared to the rest of the list (makes sense, since a good number of
those pokemon are legendaries. Let’s look at the `totalstat` breakdown
across all these pokemon.

``` r
plot10 <- ggplot(hoenn, aes(x=totalstats)) + geom_histogram() + ggtitle("Total Stat Spread") + xlab("Total Stats") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5))
plot10
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/hoenn%20type-1.png)<!-- -->

Compared to Kanto, the Hoenn region has pokemon with higher overall
stats. Lets check out the typing breakdown. Next, we want to look at the
typing breakdown and the count of each in the entire set of 151 pokemon.

``` r
plot11 <- ggplot(hoenn, aes(x=type, fill=type)) + geom_bar() + coord_flip() + ggtitle("Hoenn Type Combinations") + xlab("Types") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5))
plot11
```

![](README_files/figure-gfm/hoenn%20type%20count-1.png)<!-- -->

There are quite a few more type combinations that are introduced in this
generation, with only 135 pokemon compared to Kanto’s 151. `Water`,
`Normal`, `Psychic`, `Grass`, and `Bug` types seem to be the most
prevalent, so we’ll dive into their stats.

``` r
hoenn_spread <- filter(hoenn, type == 'water' | type == 'normal' | type == 'grass' | type == 'psychic' | type == 'bug')

plot12 <- ggplot(hoenn_spread, aes(x=type, y=totalstats, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16) + coord_flip() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + ggtitle("Total Stat Spread of Most Prevalent Types") + xlab("Total Stats") + ylab("Pokemon Type") 
plot12
```

![](README_files/figure-gfm/specific%20hoenn%20types-1.png)<!-- -->

`Water` has a very high range (primarily due to legendary pokemon like
Kyogre), but `Psychic` typing has the highest overall mean. Let’s dive
into the available moves.

``` r
h_moves <- pokeAPI("type", 10)
table2 <- table(h_moves$power, k_moves$pp)
knitr::kable(table2, format = "pipe", caption = "Power of Psychic Moves by PP")
```

|     |   1 |   5 |  10 |  15 |  20 |  25 |
|:----|----:|----:|----:|----:|----:|----:|
| 35  |   0 |   0 |   0 |   1 |   0 |   0 |
| 40  |   0 |   0 |   0 |   0 |   0 |   1 |
| 50  |   0 |   0 |   0 |   0 |   1 |   0 |
| 60  |   0 |   0 |   0 |   1 |   1 |   1 |
| 65  |   0 |   0 |   0 |   1 |   0 |   0 |
| 70  |   0 |   1 |   0 |   1 |   0 |   0 |
| 75  |   0 |   0 |   1 |   1 |   0 |   0 |
| 80  |   0 |   0 |   2 |   2 |   0 |   0 |
| 85  |   0 |   0 |   1 |   0 |   0 |   0 |
| 90  |   0 |   0 |   0 |   1 |   0 |   0 |
| 95  |   0 |   0 |   1 |   0 |   0 |   0 |
| 100 |   0 |   5 |   1 |   0 |   0 |   0 |
| 110 |   0 |   1 |   0 |   0 |   0 |   0 |
| 120 |   0 |   1 |   0 |   1 |   0 |   0 |
| 130 |   0 |   3 |   0 |   0 |   0 |   0 |
| 150 |   0 |   4 |   0 |   0 |   0 |   0 |
| 180 |   0 |   1 |   0 |   0 |   0 |   0 |

Power of Psychic Moves by PP

It seems that there are also a strong assortment of psychic moves for
the pokemon in this generation (and by extension, all generations), to
use. The types listed (specifically `Psychic` and `Bug`) have more of a
benefit with high `special attack`, and pokemon that are usually
battling against these types want high `special defense`. Let’s check to
see the correlation between the two.

``` r
plot13 <- ggplot(hoenn, aes(x=special.attack, y=special.defense)) + geom_point(stat='identity') + geom_smooth() + stat_cor(mapping = NULL, data = NULL, method = "pearson", alternative = "two.sided") + ggtitle("Special Attack v Special Defense Correlation") + xlab("Special Attack") + ylab("Special Defense") + theme(plot.title = element_text(hjust = 0.5))
plot13
```

![](README_files/figure-gfm/SpA%20v%20SpD%20hoenn-1.png)<!-- -->

The correlation is a lot stronger here, though as always, `speed` is the
most valuable stat. Let’s see how it compares across the most popular
types.

``` r
plot14 <- ggplot(hoenn_spread, aes(x=type, y=speed, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16) + coord_flip() + theme(legend.position = "none") + ggtitle("Hoenn Speed of Most Prevalent Types") + xlab("Types") + ylab("Speed Stat") + theme(plot.title = element_text(hjust = 0.5))
plot14
```

![](README_files/figure-gfm/speed%20hoenn-1.png)<!-- -->

The speed for `Psychic` types edges out the others, but the range is
quite small. All in all, psychic types such as `Metagross`, `Latios`,
`Latias`, and `Jirachi` seem to still have a lot of power in this
generation. We’re going to take a look at one last generation, just to
tie all of these results together and get a base understanding of which
type/pokemon are strong overall (not just in generation). We’re looking
at the generation that is lauded as the most popular one of all:
Generation 4.

### `The People's Champ: Sinnoh Region`

``` r
sinnoh <- pokeAPI("gen", 4)
sinnoh <- sinnoh %>% mutate(totalstats = hp+attack+defense+special.attack+special.defense+speed)
head(sinnoh)
```

    ##        name pokedex_id          type  ability weight hp attack defense special.attack special.defense speed
    ## 1   turtwig        387         grass overgrow    102 55     68      64             45              55    31
    ## 2    grotle        388         grass overgrow    970 75     89      85             55              65    36
    ## 3  torterra        389  grass/ground overgrow   3100 95    109     105             75              85    56
    ## 4  chimchar        390          fire    blaze     62 44     58      44             58              44    61
    ## 5  monferno        391 fire/fighting    blaze    220 64     78      52             78              52    81
    ## 6 infernape        392 fire/fighting    blaze    550 76    104      71            104              71   108
    ##   catch_rate_pct totalstats
    ## 1             18        318
    ## 2             18        405
    ## 3             18        525
    ## 4             18        309
    ## 5             18        405
    ## 6             18        534

Let’s look at the strongest pokemon. As always, legendaries will be a
good portion of the list, but let’s see if there are any surprising
additions.

``` r
sinnoh_strongest <- top_n(sinnoh, n=25, totalstats)

plot15 <- ggplot(sinnoh_strongest, aes(x=name, y=totalstats, fill=type)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Strongest Sinnoh Pokemon") + xlab("Pokemon Name") + ylab("Total Stats") + theme(plot.title = element_text(hjust = 0.5))
plot15
```

![](README_files/figure-gfm/sinnoh%20strongest-1.png)<!-- -->

Again, the three starters and many of the legendaries are present. I
have half a mind to not count `Arceus` since it is the literal god of
pokemon and can technically be any single type of pokemon. Many of the
pokemon on this list have unique typing or are pseudo/actual legendary
pokemon. With the even smaller pool of pokemon present that are unique
to this generation, it will be a bit tougher to analyze type.

``` r
plot16 <- ggplot(sinnoh_strongest, aes(x=name, y=catch_rate_pct, fill=type)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Strongest Sinnoh Pokemon Catch Rate") + xlab("Pokemon Name") + ylab("Catch Rate %") + theme(plot.title = element_text(hjust = 0.5))
plot16
```

![](README_files/figure-gfm/sinnoh%20catch%20rate-1.png)<!-- -->

Interestingly, `Probopass` and `Hippowdon` are the two pokemon with the
highest `catch rate`. Let’s look at the `totalstat` spread.

``` r
plot17 <- ggplot(hoenn, aes(x=totalstats))  + geom_histogram() + xlab("Total Stats") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5))
plot17
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/sinnoh%20type-1.png)<!-- -->

There’s a bit of a higher concentration of pokemon in the 400-500 range,
whereas in Hoenn there were more pokemon in the 300-400 range. Again,
this can be because of the higher proportion of legendary pokemon.

``` r
plot18 <- ggplot(sinnoh, aes(x=type, fill=type))  + geom_bar() + coord_flip() + ggtitle("Sinnoh Type Combinations") + xlab("Types") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5))
plot18
```

![](README_files/figure-gfm/sinnoh%20type%20count-1.png)<!-- -->

The spread seems a bit similar to Hoenn, with `Water`, `Normal`,
`Psychic`, `Grass`, and `Electric` being the most prevalent. Time to
dive into their stats.

``` r
sinnoh_spread <- filter(sinnoh, type == 'water' | type == 'normal' | type == 'grass' | type == 'psychic' | type == 'electric')

plot19 <- ggplot(sinnoh_spread, aes(x=type, y=totalstats, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16) + coord_flip() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + ggtitle("Total Stat Spread of Most Prevalent Types") + xlab("Total Stats")
plot19
```

![](README_files/figure-gfm/specific%20sinnoh%20types-1.png)<!-- -->

`Normal` has the highest spread but `Psychic` has the highest overall
mean for `totalstats` (even though there weren’t many observations).
Let’s check the relationship between both `attack`/`defense`/ and
`special attack`/`special defense`.

``` r
plot20 <- ggplot(sinnoh, aes(x=attack, y=defense)) + geom_point(stat='identity') + geom_smooth() + stat_cor(mapping = NULL, data = NULL, method = "pearson", alternative = "two.sided") + ggtitle("Attack v Defense Correlation") + xlab("Attack") + ylab("Defense") + theme(plot.title = element_text(hjust = 0.5))
plot20
```

![](README_files/figure-gfm/attack%20v%20defense%20sinnoh-1.png)<!-- -->

``` r
plot21 <- ggplot(sinnoh, aes(x=special.attack, y=special.defense)) + geom_point(stat='identity') + geom_smooth() + stat_cor(mapping = NULL, data = NULL, method = "pearson", alternative = "two.sided") + ggtitle("Special Attack v Special Defense Correlation") + xlab("Special Attack") + ylab("Special Defense") + theme(plot.title = element_text(hjust = 0.5))
plot21
```

![](README_files/figure-gfm/SpA%20v%20SpD%20sinnoh-1.png)<!-- -->

Lastly, let’s check the `speed` breakdown of the types we focused on.

``` r
plot22 <- ggplot(sinnoh_spread, aes(x=type, y=speed, fill=type)) + geom_boxplot(show.legend = FALSE) + geom_jitter(shape=16) + coord_flip() + theme(legend.position = "none") + ggtitle("Kanto Speed of Most Prevalent Types") + xlab("Types") + ylab("Speed Stat") + theme(plot.title = element_text(hjust = 0.5))
plot22
```

![](README_files/figure-gfm/speed%20sinnoh-1.png)<!-- -->
