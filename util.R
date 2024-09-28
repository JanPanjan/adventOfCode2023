# obrne imena in vrednosti v vektorju
# in: named_vector - vektor z imeni
# out: vektor z obrnjenimi vrednostmi
turn <- \(named_vector){
    named_vector %>%
        setNames(names(.), .)
}

# reads text file by filename
# returns vector
readFile <- \(filename) {
    file <- read.delim(filename, header = F) |>
        unlist() |>
        setNames(NULL)

    print(file)
    return(file)
}

# funkcija izpiše vrednosti v txt datoteko
writeText <- \(vrednost, filename) {
    write.table(vrednost, filename, row.names = F, col.names = F)
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

# funkcija dobi prvi digit števila (rekurzivno)
#   - mat
#   - row
#   - col
dobiPrviDigit <- \(mat, row, col) {
    # NOTE: dobili smo število, ampak ne vemo, če je levo več števk
    # gremo levo
    # ko najdemo znak, ki ni število ali če je prvi col končamo

    # base case
    if (col == 1 || !grepl("\\d", mat[row, col - 1])) {
        return(col)
    } else {
        # pomaknemo se eno levo
        return(dobiPrviDigit(mat, row, col - 1))
    }
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

# funckija izenači dolžine vektorjev, tako da doda ničle na konec
# prvi vektor naj bo krajši vektor
izenaci_vektorja <- \(prvi_vektor, drugi_vektor) {
    max_len <- length(drugi_vektor)
    cur_len <- length(prvi_vektor)

    return(c(prvi_vektor, rep(0, max_len - cur_len)))
}

#' Funckija za izračun razdalj v day 6 part 1.
#'
#' @param time int, ki predstavlja čas trenutne dirke
#' @returns numeric vector razdalj za vsak čas
getDistances <- \(time) {
    dst_trv <- 1:time

    for (i in 1:time) {
        dst_trv[i] <- dst_trv[i] * (time - i)
    }

    return(dst_trv)
}

#' funkcija dobi razdaljo za part2 day 6
#'
#' @param cur_time (integer) čas za katerega hočeš izračunat razdaljo
#' @param preostaliCas (integer) od maksimalnega časa. razlika med max_time in cur_time
getDistance <- \(cur_time, preostaliCas) {
    return(cur_time * preostaliCas))
}

#' Funkcija za izračun kombinacij iz day 6 part 1
#'
#' Dobi vse vrednosti iz distances, ki so večje kot rekord
#'
#' @param rekord integer
#' @param distances numeric vektor
#' @param value boolean, ali naj vrne vrednosti (TRUE), ali število vrednosti (FALSE)
getKombinacije <- \(rekord, distances, value = FALSE) {
    ids <- which(distances > rekord)

    ifelse(
        value,
        return(distances[ids]),
        return(length(ids))
    )
}
