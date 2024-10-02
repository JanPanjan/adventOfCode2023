library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

file <- readFile("test.txt") %>%
    as.data.frame() %>%
    separate(col = ".", into = c("hand", "rank"))

file$rank <- as.numeric(file$rank)
file

# ali lahko naredim faktor s števili in characterji?
order <- \(vec) {
    lvl <- c("A", "K", "Q", "J", "T", 9, 8, 7, 6, 5, 4, 3, 2)
    return(factor(vec, levels = lvl, ordered = F) %>% sort())
}
order(c("3", "5", "9", "Q", "T", "A"))
# lahko
