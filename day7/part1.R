library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

file <- readFile("test.txt") %>%
    as.data.frame() %>%
    separate(col = ".", into = c("hand", "bid")) %>%
    {
        .[, "bid"] <- as.numeric(.[, "bid"])
        .
    }
file

# AAAAA - five of a kind
# AAAAB - four of a kind
# AAABC - three od a kind
# AAABB - full house
# AABBC - two pair
# AABCD - one pair
# ABCDE - high card

init_cards <- \(hand) {
    cards <- rep(0, 5)
    names(cards) <- strsplit(hand, "") %>% unlist()
    cards
}

calc_occurences <- \(cards) {
    for (i in 1:length(cards)) {
        cards[names(cards)[i]] <- cards[names(cards)[i]] + 1
    }
    cards[unique(names(cards))]
}

card_counts <- map(simplify(select(file, hand)), \(hand){
    init_cards(hand) %>%
        calc_occurences()
})

# kako dobit tip karte...

#' funkcija izračuna tip roke glede na karte v roki.
#' @param frekvence table s frekvencami kart.
#' @returns integer, ki predstalvja tip karte. najmočnejši tip ima
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

# dobimo tipe od hands in posodobimo data frame
file <- map(card_counts, card_type) %>%
    mutate(file, tip = .)
