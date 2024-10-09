library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE)
source("util.R")

file <- parseData("test.txt")
file

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
    unnest(., type) %>%
    group_by(type) %>%
    arrange(type)
file

# zdaj moramo najt katere karte imajo isti tip in jih primerjat med sabo
# insert sort?...
type_3 <- filter(card_freq, type == 3)
type_3

card_strengths <- unlist(strsplit("23456789TJQKA", ""))
match("T", card_strengths)
match("K", card_strengths)

# vsaki karti dodelimo vrednost glede na njeno moč
match(unlist(strsplit("KK677", "")), card_strengths) -> r1
match(unlist(strsplit("KTJJT", "")), card_strengths) -> r2
r1
r2

# prva vrednost, ki se razlikuje, vrnemo tisti vektor, ki ima večjo število
# dobimo torej 12 12 5 6 6 in 12 9 10 10 9
# 12 in 12, 12 in 9
# ker ima prvi vektor 12 in drugi 9 in 12 > 9, vrnemo prvi hand
#
# uporabimo insertion sort >:D

# better_hand()
