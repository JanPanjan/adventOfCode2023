library(tidyverse)

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

# funkcija preveri, če je znak število
# 	- znak
isDigit <- \(znak) {
    return(str_detect(znak, "\\d"))
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
# 	- col
# 	- number length
getNum <- \(matrika, row, start, len) {
    end <- start + len - 1

    st <- matrika[row, start:end] %>%
        paste0(collapse = "") %>%
        as.numeric()

    return(st)
}

# funkcija preveri, če je niz simbol
# 	- niz ali znak
isSymbol <- \(niz) {
    regex <- c("[#|$|%|&|/|=|?|*|+|-|@]")

    return(str_detect(niz, regex))
}

# funkcija preveri, če je število validno
# 	- matrika
# 	- row
# 	- col
isNumberValid <- \(matrika, row, col) {
    numLen <- getNumLength(matrika, row, col)
    endCol <- col + numLen

	# 1. Pogledamo nad številom
	# če je prvi row, ne gremo nad število
	if (row == 1) {
		# če je prvi col, ne gremo pred število
		if (col == 1) {
			start <- col
			end <- numLen + 1
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		}
		# če je zadnji col, ne gremo za število
		else if (col == ncol(matrika)) {
			start <- col - 1
			end <- numLen
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		} else {
			aboveRow <- row - 1
			start <- col - 1
			end <- numLen + col
			niz <- matrika[aboveRow, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		}
	} 
	# 2. Pogledamo pod številom
	# če je zadnji row, ne gremo pod število
	else if (row == nrow(matrika)) {
		# če je prvi col, ne gremo pred število
		if (col == 1) {
			start <- col
			end <- numLen + 1
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		}
		# če je zadnji col, ne gremo za število
		else if (col == ncol(matrika)) {
			start <- col - 1
			end <- numLen
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		} else {
			bellowRow <- row + 1
			start <- col - 1
			end <- numLen + col
			niz <- matrika[bellowRow, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		}
	} 

	# 3. Pogledamo okoli števila
	else {
		# če je prvi col, ne gremo pred število
		if (col == 1) {
			start <- col
			end <- numLen + 1
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		}
		# če je zadnji col, ne gremo za število
		else if (col == ncol(matrika)) {
			start <- col - 1
			end <- numLen
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		} else {
			start <- col - 1
			end <- numLen + col
			niz <- matrika[row, start:end] %>% paste0(collapse = "")

			print(niz)
			return(isSymbol(niz))
		}
	}
}



# jan, to je treba polepšat.
