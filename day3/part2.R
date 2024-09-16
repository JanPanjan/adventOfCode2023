library(tidyverse)
source("util.R")

file <- readFile(filename = "data.txt")
file <- readFile(filename = "testData.txt")

# matrika ki bo hranila podatke
# za ta primer je matrika v redu, ker imamo enako dolge vrstice podatkov
mat <- makeDataMatrix(file); mat

# matrika ki bo hranila števila, ki smo jih že videli, da preprečimo dvojno štetje
mat_id <- matrix(0, nrow = 1, ncol = 3)
colnames(mat_id) <- c("row", "col", "num"); mat_id

vsota <- 0
st1   <- 0
st2   <- 0
# i in j gresta po gridu in iščeta prvo število
# row in col gresta okoli števila, da najdeta *
# x in y gresta okoli * da najdeta drugo število
for (i in seq_len(nrow(mat))) {
    print(" -----------------------")
    print("iščem prvo število")
    j <- 1

    while (j <= ncol(mat)) {
        print(paste("i:j -", i, j))
        znak <- mat[i, j]

        # če najdemo število (kot character)...
        if (isDigit(znak)) {
            # dobimo celo število (max 3 mestno)
            firstDigit <- dobiPrviDigit(mat, i, j)
            st1        <- dobiSt(mat, i, firstDigit)
            len1       <- nchar(st1)
            print(paste("našel prvo število", st1, "na indeksu", i, ":", firstDigit ,"z dolžino", len1))

            # preverimo, ali je indeks števke v matriki indeksov
            if (isInMat(mat = mat_id, row = i, col = j)) {
                print(paste("število", st1, "je v matriki indeksov. next"))
                print("- - - - -- - -  -- - - - -")
                # takoj preskočimo število
                j <- j + 1
                next
            } else {
                # dodamo števila v indeks
                print(paste("število", st1, "je novo število"))
                print("$ $ $ $ $ $")
                print("dodajanje vrednosti v matriko indeksov...")
                print(paste("dolžina števila", len1))
                mat_id <- addToMat(matId = mat_id, mat = mat, row = i, col = j, num_len = len1)
                print("$ $ $ $ $ $")

                # pogledamo vse znake okoli števila
                print("postavljanje vrednosti...")
                startRow  <- max(i - 1, 1)
                endRow    <- min(i + 1, nrow(mat))
                startCol  <- max(j - 1, 1)
                endCol    <- min(j + len1, ncol(mat))
                hasSymbol <- FALSE

                # loopamo okoli števila
                # če najdemo zvezdo...
                print("- - - - -- - -  -- - - - -")
                print("loopamo okoli števila")
                for (row in startRow:endRow) {
                    print(paste("vrstica", row))
                    for (col in startCol:endCol) {
                        znak <- mat[row, col]
                        print(paste("znak", znak))

                        if (znak == "*") {
                            print("našli zvezdo.")
                            # NOTE: loopamo okoli zvezde, da najdemo drugo število (čez 3x3 grid)
                            print("postavljanje vrednosti...")
                            start_x <- max(row - 1, 1)
                            end_x   <- min(row + 1, nrow(mat))
                            start_y <- max(col - 1, 1)
                            end_y   <- min(col + 1, ncol(mat))
                            idInMat <- FALSE

                            print("- - - - -- - -  -- - - - -")
                            print("loopamo okoli *")
                            print(paste("start_x", start_x, "in start_y", start_y))
                            print("iščem drugo število...")
                            for (x in seq(start_x, end_x)) {
                                print(paste("vrstica", x))
                                for (y in seq(start_y, end_y)) {
                                    drugiZnak <- mat[x, y]
                                    print(paste("znak '", mat[x,y], "'"))

                                    # NOTE: če najdemo število, preverimo, ali je indeks v matriki
                                    if (isDigit(drugiZnak)) {
                                        prviDigit <- dobiPrviDigit(mat, x, y)
                                        st2       <- dobiSt(mat, x, prviDigit)
                                        len2      <- nchar(st2)

                                        if (isInMat(mat_id, x, y)) {
                                            print(paste("skipamo iteracijo okoli zvezde. znak", mat[x,y], "je v matriki"))
                                            next
                                        }
                                        print(paste("našel drugo število", st2, "na indeksu", x, ":", y ,"z dolžino", len2))

                                        # dodamo ga v matriko
                                        print(paste("število", st2, "je novo število"))
                                        print("$ $ $ $ $ $")
                                        print("dodajanje vrednosti v matriko indeksov...")
                                        mat_id <- addToMat(matId = mat_id, mat = mat, row = x, col = prviDigit, num_len = len2)
                                        print("$ $ $ $ $ $")

                                        # posodobimo vsoto s st1 in st2
                                        print(paste("dodajam",st1,"x",st2,"v vsoto"))
                                        print(paste("začetna vsota:", vsota))
                                        vsota <- vsota + st1 * st2
                                        print(paste("nova vsota:", vsota))

                                        idInMat <- TRUE
                                        j <- j + 1
                                        # exitamo y
                                        break
                                    } # isDigit drugo število
                                } # y
                                if (idInMat) {
                                    # breakamo iz x
                                    break
                                }
                            } # x

                            # ko končamo z iskanjem drugega števila, exitamo loopa
                            hasSymbol <- TRUE

                            # exitamo col
                            break
                        } # znak == * ?
                    } # col
                    if (hasSymbol) {
                        # exitamo row
                        break
                    }
                } # row
            j <- j + 1
            } # isInMat prvo število
        } # isDigit prvo število
        else {
            # gremo na naslednji column
            j <- j + 1
        }
        print("- - - - -- - -  -- - - - -")
    } # j
    print("konec vrstice.")
} # i

write.table(vsota, "odgovor2.txt", row.names = F, col.names = F)











# ja, nikoli več ne bom nestal 30 for loops in tak
# grozno je blo debuggat
