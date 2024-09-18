library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

file <- readFile("testData.txt")
file <- readFile("data.txt")

# [[ PARSING DATA ]]

# vrne list
parseData <- \(file) {
    # odstranimo vse pred ":" in splitamo glede na "|"
    data <- sub(".*: ", "", file) %>%
        str_split(., " \\| ") %>%
        unlist()

    # naredimo seznama števil
    ourNums <- c()
    winNums <- c()

    for (i in seq(2, length(data), 2)) {
        # še enkrat splittamo seznama, da dobimo posamezna števila
        winNums <- c(winNums, str_split(data[i - 1], " "))
        ourNums <- c(ourNums, str_split(data[i], " "))
    }

    # odstranimo NA vrednosti (prazni strings)
    map(winNums, grep, pattern = "\\d", value = TRUE) -> winNums
    map(ourNums, grep, pattern = "\\d", value = TRUE) -> ourNums

    # pretvorimo v numeric
    map(winNums, as.numeric) -> winNums
    map(ourNums, as.numeric) -> ourNums

    # [[1]] - ourNums
    # [[2]] - winNums
    return(list("ourNums" = ourNums, "winNums" = winNums))
}

parseData(file)[["ourNums"]] -> ourNums
ourNums
parseData(file)[["winNums"]] -> winNums
winNums

# ker so winNums prekratki, jih moramo nafilati z 0 do dolžine ourNums
# (v util je funkcija izenaci_vektorja)
map(winNums, izenaci_vektorja, drugi_vektor = ourNums[[1]]) -> winNums

winNums
ourNums

# [[ MAIN DEL ]]

# hranimo števila kart
# začnemo z eno kopijo vsake
# cardCount <- rep(1, length(winNums))
# names(cardCount) <- as.character(seq(1:length(winNums)))

# primerjamo vsako število iz ourNums z vsakim iz winNums
# če najdemo match, se posodobi vrednost v vektorju cardCount glede na število matchev

# npr. card 1 ima 4 matche
# to pomeni, da se poveča število naslednjih štirih za 1 * število kopij trenutne karte
# i <- 1
# matchCount <- 4
# cardCount[(i+1):(i+matchCount)] <- cardCount[(i+1):(i+matchCount)] + (1 * cardCount[i])
# card 2 bo imel zdaj 2 kopije, torej mora dvakrat opraviti increment
# ima 2 matcha, torej se 2x poveča za 1, torej +2
# i <- 2
# matchCount <- 2
# cardCount[(i+1):(i+matchCount)] <- cardCount[(i+1):(i+matchCount)] + (1 * cardCount[i])
# na koncu seštejemo vse kopije
# sum(cardCount)

cardCount <- rep(1, length(winNums))
names(cardCount) <- as.character(seq(1:length(winNums)))

add_copies <- \(i, matchCount, cardCount) {
    if (matchCount == 0) {
        return(cardCount)
    }

    cardCount[(i + 1):(i + matchCount)] <- cardCount[(i + 1):(i + matchCount)] + (1 * cardCount[i])
    return(cardCount)
}

# moramo it čez vsak element lista
# 1. gremo čez vsak vektor od ourNums
# 2. najdemo število matchev
# 3. prištejemo kopije glede na zgornji algoritem
# 4. seštejemo vse kopije
for (i in seq_len(length(winNums))) {
    matchCount <- 0
    print("---------------------------")
    print(paste("card", i))

    print("comparing nums...")
    for (j in seq_len(length(winNums[[i]]))) {
        ourNums[[i]][j] == winNums[[i]] -> l

        # vrne vektor z indeksi
        # če ni matcha, vrne prazen vektor
        grep(x = l, pattern = TRUE) -> rezultat

        if (length(rezultat) > 0) {
            # naj bi vrnilo samo en true value
            # zato večamo samo za 1 match
            matchCount <- matchCount + 1
        }
    }

    print(paste("matchCount", matchCount))
    add_copies(i = i, matchCount = matchCount, cardCount = cardCount) -> cardCount
}
sum(cardCount) -> end

# write.table(total, "odgovor2Test.txt", row.names = F, col.names = F)
write.table(end, "odgovor2.txt", row.names = F, col.names = F)
