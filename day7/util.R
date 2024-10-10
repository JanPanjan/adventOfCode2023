#' prebere podatke in naredi data frame (day 7)
#' @param filename path do datoteke s podatki
#' @returns data frame
parseData <- \(filename) {
    readFile(filename) %>%
    as.data.frame() %>%
    separate(col = ".", into = c("hand", "bid")) %>%
    {
        .[, "bid"] <- as.numeric(.[, "bid"])
        .
    }
}

#' naredi vektor kart, da bomo lahko prešteli frekvence
#' @param hand string s kartami
#' @returns named character vector
init_cards <- \(hand) {
    cards <- rep(0, 5)
    names(cards) <- strsplit(hand, "") %>% unlist()
    cards
}

#' funkcija prešteje kolikokrat se pojavi posamezna karta
#' ohrani samo vektor unikatnih kart s ponovitvami
#'   npr. lahko se pojavita 2 trojki - ohrani samo eno v rezultatu
#' @param cards character vector
#' @returns cleaned named vector frekvenc
calc_occurences <- \(cards, part_two = F) {
    for (i in 1:length(cards)) {
        cards[names(cards)[i]] <- cards[names(cards)[i]] + 1
    }

    cards[unique(names(cards))]
}

#' funkcija izračuna tip roke glede na karte v roki.
#' @param frekvence vektor s frekvencami kart (rezultat calc_occurences)
#' @returns integer, ki predstavlja tip karte. najmočnejši tip ima
#' vrednost 7 (five of a kind), medtem ko najšibkejši 1 (high card).
#' - five of a kind. ni unikatnih kart
#' - four of a kind. imamo karto ki se ponovi 4x
#' - full house. imamo dve unikatni karti
#' - three of a kind. imamo karto ki se ponovi 3x
#' - two pair. imamo 3 unikatne karte
#' - one pair. imamo 4 unikatne karte
#' - high card. vse karte so unikatne
card_type <- \(frekvence) {
    case_when(
        length(frekvence) == 1 ~ 7,
        max(frekvence) == 4 ~ 6,
        length(frekvence) == 2 ~ 5,
        max(frekvence) == 3 ~ 4,
        length(frekvence) == 3 ~ 3,
        length(frekvence) == 4 ~ 2,
        length(frekvence) == 5 ~ 1,
        .default = 0
    )
}

#' funkcija pripiše karti določeno moč glede na podano lestvico
#' @param hand string
#' @returns numeric vector
get_card_strengths <- \(hand, part_two = FALSE) {
    card_strengths <- ifelse(
        part_two == TRUE,
        yes = unlist(strsplit("J23456789TQKA", "")),
        no = unlist(strsplit("23456789TJQKA", ""))
    )
    match(unlist(strsplit(hand, "")), card_strengths)
}


# zamenjamo jokerje
map_jokers <- \(hand) {
    if ("J" %in% names(hand) & length(hand) > 1) {
        # dobimo karto ki se ponovi največkrat in ni joker
        card_to_replace <- hand[names(hand) != "J"] %>% 
            { names(.[. == max(.)]) }
        # zamenjamo jokerje s to karto
        names(hand) <- gsub("J", card_to_replace, names(hand))
        return(hand)
    } else {
        return(hand)
    }
}
