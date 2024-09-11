library(tidyverse)
source("util.R")

# read file
file <- readFile(filename="testData.txt"); file

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file); mat
# matrika ki bo hranila indekse simbolov
ids <- getSymbols(mat) |> optimizeMatrix(); ids
