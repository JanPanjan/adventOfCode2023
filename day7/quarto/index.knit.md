---
author: "jan panjan"
---


# Packages and util functions


::: {.cell}

```{.r .cell-code}
library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE)
```
:::


# Parsing data

Naredimo data frame it podatkov v `test.txt`.


::: {.cell}

```{.r .cell-code}
file <- readFile("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/day7/test.txt") %>%
    as.data.frame() %>%
    separate(col = ".", into = c("hand", "bid")) %>%
    {
        .[, "bid"] <- as.numeric(.[, "bid"])
        .
    }
file
```

::: {.cell-output .cell-output-stdout}

```
   hand bid
1 32T3K 765
2 T55J5 684
3 KK677  28
4 KTJJT 220
5 QQQJA 483
```


:::
:::


S funkcijami bomo uredili naš data frame do konca. Najprej bomo izračunali
kolikokrat se ponovi ista karta v roki, nato dobili tip roke.

Obstaja 7 tipov:

1. five of a kind
2. four of a kind
3. three of a kind
4. full house
5. two pair
6. one pair
7. high card

:::panel-tabset

# init_cards


::: {.cell}

```{.r .cell-code}
#' naredi vektor kart, da bomo lahko prešteli frekvence
#'
#' @param hand string s kartami
#' @returns named character vector
init_cards <- \(hand) {
    cards <- rep(0, 5)
    names(cards) <- strsplit(hand, "") %>% unlist()
    cards
}
```
:::


# calc_occurences


::: {.cell}

```{.r .cell-code}
#' funkcija prešteje kolikokrat se pojavi posamezna karta
#' ohrani samo vektor unikatnih kart s ponovitvami
#'   npr. lahko se pojavita 2 trojki - ohrani samo eno v rezultatu

#' @param cards character vector
#' @return cleaned named vector frekvenc
calc_occurences <- \(cards) {
    for (i in 1:length(cards)) {
        cards[names(cards)[i]] <- cards[names(cards)[i]] + 1
    }
    cards[unique(names(cards))]
}
```
:::


# card_type


::: {.cell}

```{.r .cell-code}
#' funkcija izračuna tip roke glede na karte v roki.

#' @param frekvence vektor s frekvencami kart (rezultat calc_occurences)
#' @return integer, ki predstavlja tip karte. najmočnejši tip ima
#' vrednost 7 (five of a kind), medtem ko najšibkejši 1 (high card).
card_type <- \(frekvence) {
    case_when(
        # five of a kind. ni unikatnih kart
        length(frekvence) == 1 ~ 7,

        # four of a kind. imamo karto ki se ponovi 4x
        max(frekvence) == 4 ~ 6,

        # full house. imamo dve unikatni karti
        length(frekvence) == 2 ~ 5,

        # three of a kind. imamo karto ki se ponovi 3x
        max(frekvence) == 3 ~ 4,

        # two pair. imamo 3 unikatne karte
        length(frekvence) == 3 ~ 3,

        # one pair. imamo 4 unikatne karte
        length(frekvence) == 4 ~ 2,

        # high card. vse karte so unikatne
        length(frekvence) == 5 ~ 1,

        # safety
        .default = 0
    )
}
```
:::


Zdaj to pipamo in posodobimo naš dataframe.


::: {.cell}

```{.r .cell-code}
# dobimo frekvence kart
file <- map(simplify(select(file, hand)), \(hand){
    init_cards(hand) %>%
        calc_occurences() %>%
        setNames(NULL)
}) %>%
    # dobimo tipe od hands
    map(., card_type) %>%
    {
        names(.) <- NULL
        .
    } %>%
    mutate(file, type = .) %>%
    unnest(., type)

file %>%
    group_by(type)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 5 × 3
# Groups:   type [3]
  hand    bid  type
  <chr> <dbl> <dbl>
1 32T3K   765     2
2 T55J5   684     4
3 KK677    28     3
4 KTJJT   220     3
5 QQQJA   483     4
```


:::
:::


:::

Zdaj moramo najt katere karte imajo isti tip in jih primerjat med sabo

