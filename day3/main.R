library(tidyverse)
source("util.R")

# read file
file <- readFile(filename = "testData.txt")

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file)

# matrika ki bo hranila indekse simbolov
ids <- getSymbolIndexes(mat)

checkHorizontalLeft(matrika = mat, simbolIndeks = ids[3, ])
checkHorizontalRight(matrika = mat, simbolIndeks = ids[3, ])
