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
    unnest(., type)
file

# zdaj moramo najt katere karte imajo isti tip in jih primerjat med sabo

get_card_strengths <- \(hand) {
    card_strengths <- unlist(strsplit("23456789TJQKA", ""))
    match(unlist(strsplit(hand, "")), card_strengths)
}

# prva vrednost, ki se razlikuje, vrnemo tisti vektor, ki ima večjo število
# dobimo torej 12 12 5 6 6 in 12 9 10 10 9
# 12 in 12, 12 in 9
# ker ima prvi vektor 12 in drugi 9 in 12 > 9, vrnemo prvi hand
file$card_strengths <- map(file$hand, get_card_strengths)

file %>% 
    group_by(type) %>% 
    arrange(type)

# zdaj imamo urejen data frame
# prvi element je najmočnejša roka
# zadnji element je najšibkejša roka
# moramo dobiti še rezultat:
#   row number × bid amount
file
rows <- 1:nrow(file)
prod <- unlist(select(file, bid)) * rows
prod[3]/3
1932/4
3420/5

writeText(result, "odgovorTest.txt")
