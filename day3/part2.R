library(tidyverse)
source("util.R")

# read file
file <- readFile(filename = "testData.txt")

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file)

# matrika ki bo hranila števila, ki smo jih že videli, da preprečimo dvojno štetje
mat_id <- matrix(0, nrow = 1, ncol = 3)
colnames(mat_id) <- c("row", "col", "num"); mat_id

st1 <- 0
st2 <- 0
for (i in seq_len(nrow(mat))) {
    j <- 1

    while (j <= ncol(mat)) {
        znak <- mat[i, j]

        # če najdemo število (kot character)...
        if (isDigit(znak)) {
            # dobimo celo število (max 3 mestno)
            st1 <- dobiSt(mat, i, j)
            len1 <- nchar(st1)
            print(st1)

            # preverimo, ali je indeks števke v matriki indeksov
            if (isInMat(mat = mat_id, row = i, col = j)) {
                # takoj preskočimo število
                j <- j + len1
                next
            } else {
                # dodamo indeks
                mat_id <- addToMat(mat_id, i, j, st1)

                # pogledamo vse znake okoli števila
                startRow <- max(i - 1, 1)
                endRow <- min(i + 1, nrow(mat))
                startCol <- max(j - 1, 1)
                endCol <- min(j + len1, ncol(mat))
                hasSymbol <- FALSE

                # loopamo okoli števila
                # če najdemo zvezdo...
                for (row in startRow:endRow) {
                    for (col in startCol:endCol) {
                        znak <- mat[row, col]
                        print(paste("znak", col, znak))

                        if (znak == "*") {
                            # loopamo okoli zvezde, da najdemo drugo število (čez 3x3 grid)
                            start_x <- max(row - 1, 1)
                            end_x <- min(row + 1, nrow(mat))
                            start_y <- max(col - 1, 1)
                            # end_y <- min(row + len, ncol(mat))
                            end_y <- min(col + 1, ncol(mat))
                            idInMat <- FALSE

                            for (x in seq(start_x, end_x)) {
                                for (y in seq(start_y, end_y)) {
                                    # skippamo prvo iteracijo, ker je zadnja števka st1
                                    if (y == start_y && x == start_x) {
                                        next
                                    }

                                    drugiZnak <- mat[x, y]
                                    print(paste("drugiznak", y, drugiZnak))

                                    # če najdemo število, preverimo, ali je indeks v matriki
                                    if (isDigit(drugiZnak)) {
                                        st2 <- dobiSt(mat, x, y)
                                        len2 <- nchar(st2)

                                        if (isInMat(mat_id, x, y)) {
                                            # če je, preskočimo število (j <- j + len)
                                            idInMat <- TRUE
                                            break
                                        } else {
                                            # če ni, ga dodamo v matriko in nastavimo indeks števila kot pivot
                                            mat_id <- addToMat(mat_id, x, y, st2)
                                            # dodamo v sum
                                            sum <- sum + (st1 * st2)
                                            print(paste("sum", sum))
                                            j <- j + len2
                                        }
                                    }
                                }
                                if (idInMat) {
                                    break
                                }
                            }

                            # ko končamo z iskanjem drugega števila, exitamo loopa
                            hasSymbol <- TRUE
                            break
                        }
                    }
                    if (hasSymbol) break
                }
            }
            # naredimo vse in skipamo do konca števila
            j <- j + len1
        } else {
            # če ni število, gremo na naslednji column
            j <- j + 1
        }
    }
}
# na koncu množimo in seštejemo vsa števila shranjena v mat_id
mat
mat_id

# ker imamo sodo mnogo števil in moramo množiti dve ki ju najdemo skupaj,
# lahko vsak i in i+1 element množimo skupaj in dodamo v sum
sum <- 0
for (i in seq(2,nrow(mat_id)-1,2)) {
    sum <- sum + (mat_id[i, 3] * mat_id[i+1, 3])
}
sum

      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
 [1,] "4"  "6"  "7"  "."  "."  "1"  "1"  "4"  "."  "."
 [2,] "."  "."  "."  "*"  "."  "."  "."  "."  "."  "."
 [3,] "."  "."  "3"  "5"  "."  "."  "6"  "3"  "3"  "."
 [4,] "."  "."  "."  "."  "."  "."  "#"  "."  "."  "."
 [5,] "6"  "1"  "7"  "*"  "."  "."  "."  "."  "."  "."
 [6,] "."  "."  "."  "."  "."  "+"  "."  "5"  "8"  "."  
 [7,] "."  "."  "5"  "9"  "2"  "."  "."  "."  "."  "."
 [8,] "."  "."  "."  "."  "."  "."  "7"  "5"  "5"  "."
 [9,] "."  "."  "."  "$"  "."  "*"  "."  "."  "."  "."
[10,] "."  "6"  "6"  "4"  "."  "5"  "9"  "8"  "."  "."
