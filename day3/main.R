library(tidyverse)
source("util.R")

# read file
file <- readFile(filename = "testData.txt")

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file)

# v bistvu, bomo iskali števila. če najdemo simbol okoli števila, ga dodamo.
for (i in 1:ncol(mat)) {
    for (j in 1:nrow(mat)) {
        niz <- mat[i, j]
        if (isDigit(mat[i, j])) {
            print(niz)
            print(isNumberValid(mat, i, j))
            # sus behaviour
        }
    }
}
