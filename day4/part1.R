library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

file <- readFile("testData.txt")
file <- readFile("data.txt")

# [[ PARSING DATA ]]

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

ourNums
winNums

# [[ MAIN DEL ]]

# primerjamo vsako število iz ourNums z vsakim iz winNums
# če najdemo match, povečamo total (množimo z 2)
# ker so winNums prekratki, jih moramo nafilati z 0 do dolžine ourNums

# prvi vektor naj bo daljši vektor
izenaci_vektorja <- \(prvi_vektor, drugi_vektor) {
    max_len <- length(drugi_vektor)
    cur_len <- length(prvi_vektor)

    return(c(prvi_vektor, rep(0, max_len - cur_len)))
}

map(winNums, izenaci_vektorja, drugi_vektor = ourNums[[1]]) -> winNums
winNums

# hranimo nek total sum, ki se posodablja skozi vsako igro
total <- 0

ourNums
winNums
# primerjamo vrednosti
# moramo it čez vsak element lista
for (i in seq_len(length(winNums))) {
    # hranimo nek temporary sum tock
    print("---------------------------------------")
    print("setting tmp_sum to 0.5")
    tmp_sum <- 0.5
    first_match <- FALSE
    print(paste("card", i))

    for (j in seq_len(length(winNums[[i]]))) {
        print(paste("j:", j))
        # print(ourNums[[i]][j])
        # print(winNums[[i]])
        # print(ourNums[[i]][j] == winNums[[i]])

        print("comparing nums...")
        print(ourNums[[i]][j])
        print(winNums[[i]])
        ourNums[[i]][j] == winNums[[i]] -> l
        print(l)

        # vrne vektor z indeksi
        # če ni matcha, vrne prazen vektor
        grep(x = l, pattern = TRUE) -> rezultat
        print(paste("results:", rezultat))

        if (length(rezultat) > 0) {
            first_match <- TRUE
            tmp_sum <- tmp_sum * 2
        }

        print(paste("tmp_sum", tmp_sum))
    }

    # če nismo matchali z nobenim, ne prištejemo nič
    if (!first_match) {
        print("match not found. setting tmp_sum to 0")
        tmp_sum <- 0
    }

    total <- sum(total, tmp_sum)
    print(paste("total", total))
}

write.table(total, "odgovorTest.txt", row.names = F, col.names = F)
write.table(total, "odgovor1.txt", row.names = F, col.names = F)
