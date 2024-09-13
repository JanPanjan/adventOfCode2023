library(tidyverse)
source("util.R")

# read file
file <- readFile(filename = "mainData.txt")

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file)

# v bistvu, bomo iskali števila. če najdemo simbol okoli števila, ga dodamo.
sum <- 0

# gremo čez števila
for (i in seq_len(nrow(mat))) {
    j <- 1
    while (j <= ncol(mat)) {
        znak <- mat[i, j]

        # če najdemo število (kot character)...
        if (isDigit(znak)) {
            # Get the full number
            st <- dobiSt(mat, i, j)
            len <- nchar(st)

            # pogledamo vse znake okoli števila
            startRow <- max(i - 1, 1)
            endRow <- min(i + 1, nrow(mat))
            startCol <- max(j - 1, 1)
            endCol <- min(j + len, ncol(mat))
            hasSymbol <- FALSE

            # Loop over the 3x3 grid surrounding the number
            for (row in startRow:endRow) {
                for (col in startCol:endCol) {
                    znak <- mat[row, col]
                    if (isSymbol(znak)) {
                        hasSymbol <- TRUE
                        break
                    }
                }
                if (hasSymbol) break
            }

            # Dodamo v sum
            if (hasSymbol) {
                sum <- sum + st
            }

            # skipamo čez število
            j <- j + len
        } else {
            # če ni število, gremo na naslednji column
            j <- j + 1
        }
    }
}

write.table(sum, "odgovor1.txt", row.names = F, col.names = F)
