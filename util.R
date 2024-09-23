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

imena <- c("seed-soil", "soil-fert", "fert-water", "water-light", "light-temp", "temp-hum", "hum-loc")
# funkcija vrne vektor indeksov, kjer so imena od maps (npr. "seed-to-soil map:")
# seeds ([1]) se ne shrani, ker filtriramo glede na "\\d"
dobiMaps <- \(file) {
    # moramo dobit vsak map iz podatkov
    # nimamo praznih vrstic, moramo prebrati vrstice, dokler ne najdemo naslednji map
    maps <- map(file, grepl, pattern = "\\d") %>%
        unlist() %>%
        setNames(., c(1:length(.))) %>%
        grep(x = ., pattern = FALSE) %>%
        setNames(., imena)

    return(maps)
}

# vzamemo od indeksa v vektorju, do naslednjega indexa v vektorju
# če je zadnji indeks, vzamemo do konca

# funkcija vrne naše vrednosti od maps (integerje)
# destination, source in range
# vsak map ima lahko več takih intervalov
# vrne list, kjer je vsak element en map in pod-elementi so ti intervali
dobiMapVrednosti <- \(maps, file) {
    # vzamemo od indeksa v vektorju, do naslednjega indexa v vektorju
    # če je zadnji indeks, vzamemo do konca
    intervali <- list()

    for (i in seq(1, length(maps))) {
        if (i == length(maps)) {
            intervali[[i]] <- file[(maps[i] + 1):length(file)]
        } else {
            intervali[[i]] <- file[(maps[i] + 1):(maps[i + 1] - 1)]
        }
    }

    names(intervali) <- imena
    return(intervali)
}

# funkcija vrne data frame iz vrednosti elementa v listu
# naredi tri stolpce: dst, src in range za map
narediDataFrame <- \(map, name) {
    df <- data.frame(ena = unlist(map)) %>%
        {
            row.names(.) <- NULL
            .
        } %>%
        separate(col = ena, into = c("dst", "src", "range"), sep = " ")

    return(df)
}

# funkcija naredi list z data frames, ki so naši maps
dobiDataFrames <- \(maps_values) {
    dfs <- list()

    for (i in 1:length(names(maps_values))) {
        dfs[[i]] <- narediDataFrame(maps_values[[i]], names(maps_values)[i])
    }

    names(dfs) <- imena
    return(dfs)
}
