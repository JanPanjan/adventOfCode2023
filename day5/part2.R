library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)
source("fileFun.R", chdir = TRUE, echo = TRUE)

# file <- readFile("testData.txt")
file <- readFile("data.txt")
file <- readFile("testData.txt")

dfs <- parseData(file)
dfs

# dobimo semena
seeds <- file[1] %>%
    sub(".*: ", "", .) %>%
    str_split_1(" ") %>%
    as.numeric()
seeds

seme <- 130000:1500000000
ab <- 1:1000000000
xy <- 10:15
# 2 line
validSeme <- seme < 15 & seme >= 3
length(which(validSeme == T))
# 1 line
which(ab %in% seme)
indeksi <- c(which(seme >= ab[1]), which(seme < ab[length(ab)]))
indeksi <- seme >= ab[1] && seme < ab[length(ab)]
indeksi

seme >= ab[1] -> more
seme < ab[length(ab)] -> less
more == less

for (i in 1:length(seme)) {
    if (low == -1 && seme[i] >= ab[1]) {
        print(paste("low is", seme[i]))
        low <- seme[i]
    }
    if (!(seme[i] < ab[length(ab)])) {
        print(paste("seme", seme[i], "breaks the loop"))
        print(paste("hi is", seme[i-1]))
        hi <- seme[i-1]
        break
    }
}

fg <- 1:10
fg > 5 -> en
fg < 9 -> dv
en == dv

seeds[ab[1]:ab[length(ab)]] -> indeksi
ab[1]
ab[length(ab)]
indeksi
xy[indeksi]
