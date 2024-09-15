# reads text file by filename
# returns vector
readFile <- \(filename) {
    file <- read.delim(filename, header = F) |>
        unlist() |>
        setNames(NULL)

    print(file)
    return(file)
}

# makes matrix by spliting every character in vec
makeDataMatrix <- \(vec) {
    listData <- strsplit(vec, split = "")
    len <- length(listData)
    mat <- matrix(0, len, len)

    for (i in 1:len) {
        for (j in 1:len) {
            mat[i, j] <- listData[[i]][j]
        }
    }

    print(mat)
    return(mat)
}

# funkcija doda vrednosti v matriko indeksov
#   - matId
#   - mat
#   - row
#   - col
#   - num_len
addToMat <- \(matId, mat, row, col, num_len) {
    num_len <- num_len - 1
    for (i in 0:num_len) {
        noviCol <- col + i
        print(paste("znak, ki bo dodan:", mat[row, noviCol]))

        matId <- rbind(matId, c(row, noviCol, as.numeric(mat[row, noviCol])))
    }

    return(matId)
}

# funkcija preveri, če je indeks v matriki
#   - mat
#   - row
#   - col
isInMat <- \(mat, row, col) {
    for (i in seq_len(nrow(mat))) {
        if (mat[i, 1] == row && mat[i, 2] == col) {
            return(TRUE)
        }
    }

    return(FALSE)
}

# funcija vrne dolžino trenutnega števila
# indeks naj bo vektor row in col v matriki
# 	- matrika
# 	- row
# 	- col
getNumLength <- \(matrika, row, col) {
    st <- ""

    for (i in 0:2) {
        if (col == ncol(matrika)) {
            return(1)
        }

        znak <- matrika[row, col + i]

        if (!isDigit(znak = znak)) {
            break
        }

        st <- paste0(st, znak, collapse = "")
    }

    return(nchar(st))
}

# funckija dobi število
# 	- matrika
# 	- row
# 	- col (start)
# 	- number length
getNum <- \(matrika, row, start, len) {
    end <- start + len - 1

    st <- matrika[row, start:end] %>%
        paste0(collapse = "") %>%
        as.numeric()

    return(st)
}

# funkcija dobi število v matriki
# 	- mat (matrika)
# 	- row
# 	- col
dobiSt <- \(mat, row, col) {
    st <- mat[row, col]
    start <- col + 1

    if (start > ncol(mat)) {
        return(as.numeric(st))
    }

    for (i in seq(start, ncol(mat))) {
        if (str_detect(mat[row, i], "\\d")) {
            st <- paste0(st, mat[row, i])
        } else {
            break
        }
    }

    return(as.numeric(st))
}

# funkcija preveri, če je niz simbol
# 	- niz ali znak
isSymbol <- function(niz) {
    regex <- "[#\\$%&/=\\?\\*\\+\\-@]" # Excludes dot (.)
    return(str_detect(niz, regex))
}

# funkcija preveri, če je znak število
# 	- znak
isDigit <- \(znak) {
    return(str_detect(znak, "\\d"))
}
