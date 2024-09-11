library(tidyverse)
source("util.R")

# read file
file <- readFile(filename = "testData.txt")

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file)

# matrika ki bo hranila indekse simbolov
ids <- getSymbolIndexes(mat)

mat[1, ] %>%
    paste0(collapse = "") %>%
    str_sub(-1, 2)

getPremik(mat, 1)
ids[2, ]
checkHorizontalLeft(matrika = mat, simbolIndeks = ids[2, ])
mat
ids
getNumber("46")
regmatches(x = "...", gregexpr(pattern = "\\d", text = "...")) %>%
    unlist() %>%
    paste0(collapse = "") %>%
    as.numeric()
getNumber("...")
