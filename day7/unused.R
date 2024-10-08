#' funkcija prevede vse karte v njihovo moč (torej A = 13,... 3 = 3,...)
#' @param hand string roka kart iz podatkov
#' @returns numeric vektor
card_strength <- \(hand){
    cards <- unlist(strsplit("23456789TJQKA", ""))
    match(unlist(strsplit(hand, "")), cards) + 1
}

# dobimo moč vsake karte in naredimo table
# funkcija table vrne frekvence elementov
frekvence <- card_strength("32T3K") %>% table()
