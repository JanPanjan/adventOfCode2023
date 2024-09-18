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
izenaci_vektorja <- \(prvi_vektor, drugi_vektor) {
    max_len <- 0
    cur_len <- 0

    if (length(prvi_vektor) > length(drugi_vektor)) {
        max_len <- length(prvi_vektor)
        cur_len <- length(drugi_vektor)

        return(c(drugi_vektor, rep(0, max_len - cur_len)))
    } else {
        max_len <- length(drugi_vektor)
        cur_len <- length(prvi_vektor)

        return(c(prvi_vektor, rep(0, max_len - cur_len)))
    }
}

map(ourNums, izenaci_vektorja, drugi_vektor = winNums[[1]]) -> winNums

# hranimo nek total sum, ki se posodablja skozi vsako igro
total <- 0

# primerjamo vrednosti
# moramo it čez vsak element lista
for (i in 1:length(winNums)) {
	# hranimo nek temporary sum tock
	tmp_sum <- 0.5
	first_match <- FALSE
	print(paste("card", i))
	for (j in 1:length(winNums[[i]])) {
		print(paste("j:", j))
		#print(ourNums[[i]][j])
		#print(winNums[[i]])
		#print(ourNums[[i]][j] == winNums[[i]])

		ourNums[[i]][j] == winNums[[i]] -> l

		grep(x = l, pattern = TRUE) -> rezultat
		if (length(rezultat) > 0) {
			first_match <- TRUE
			tmp_sum <- tmp_sum * 2
		}
		
		print(paste("tmp_sum", tmp_sum))
	}

	if (!first_match) {
		tmp_sum <- 1
	}
	total <- sum(total, tmp_sum)
	print(paste("total", total))
}

write.table(total, "odgovorTest.txt", row.names = F, col.names = F)
write.table(total, "odgovor1", row.names = F, col.names = F)
