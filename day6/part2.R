library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)
source("data-parse-funcs.R", chdir = TRUE, echo = TRUE)

# get data
# file <- readFile("data.txt")
file <- readFile("data.txt")
data <- parseDataAgain(file)

# do something
# ...

# če najdemo en border, najdemo drugega avtomatsko

findDistances(data) -> meja

# našli smo border na i-tem mestu
# vemo da se iz obeh smeri ponavljajo številke (glej idea.md)
# od vseh sekund odštejemo 2-krat čas, ki premaga rekord, ker je zrcalno
res <- data$time - (2 * meja) + 1

writeText(res, "odgovor2.txt")
