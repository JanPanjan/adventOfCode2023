library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE)
source("util.R")

file <- parseData("data.txt")

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

# v card_strengths shranimo števila, ki predstavljajo moč rok
file$card_strengths <- map(file$hand, get_card_strengths_p1) %>% 
    # izenačimo števila, da bo potem sortiralo pravilno
    map(., \(card){ifelse(nchar(card) == 1, paste0(0, card), paste0(card)) %>% 
            paste(., collapse = "") %>% 
            as.numeric()
        }) %>% 
    unlist()

# uredimo po velikosti znotraj skupin
file <- file %>% 
    group_by(type) %>% 
    arrange(card_strengths, .by_group = T)

# dobimo še zadnji izračun
# (bid * rank) 765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5
# ker je rank == row number, lahko vsak bid množimo s corresponding row number
prod <- {file$bid * 1:nrow(file)} %>% sum()

writeText(prod, "odgovor1.txt")
write.table(file, "part1-df.txt")
