library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

file <- readFile("test.txt") %>%
    as.data.frame() %>%
    separate(col = ".", into = c("hand", "rank")) %>%
    {
        .[, "rank"] <- as.numeric(.[, "rank"])
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
        calc_occurences() %>%
        sort(decreasing = T)
})

# kako dobit tip karte...
