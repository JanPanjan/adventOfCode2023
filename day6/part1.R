library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)
source("data-parse-funcs.R", chdir = TRUE, echo = TRUE)

# get data
file <- readFile("data.txt")
df <- parseData(file)

# do something
# ...
res <- c()

for (i in 1:nrow(df)) {
    distances <- getDistances(df[i, 1])
    kombinacije <- getKombinacije(df[i, 2], distances)

    res <- c(res, kombinacije)
}

writeText(prod(res), "odgovor1.txt")
