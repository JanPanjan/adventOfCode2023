library(tidyverse)

# reads text file by filename
# returns vector
readFile <- \(filename) {
	file <- read.delim(filename, header=F) |>
		unlist() |>
		setNames(NULL)

	print(file)
	return(file)
}

# makes matrix by spliting every character in vec
makeDataMatrix <- \(vec) {
	listData <- strsplit(vec, split="")
	len <- length(listData)
	mat <- matrix(0,len,len)

	for (i in 1:len) {
		for (j in 1:len) {
			mat[i,j] <- listData[[i]][j]
		}
	}

	print(mat)
	return(mat)
}

# funkcija vrne matriko indeksov od vseh validnih simbolov
getSymbolIndexes <- \(dataMatrix) {
	regex <- c("[#|$|%|&|/|=|?|*|+|-|@]")
	rows  <- nrow(dataMatrix)
	cols  <- ncol(dataMatrix)
	idMatrix <- matrix(0,2*rows,2)

	for (i in 1:rows) {
		for (j in 1:cols) {
			if (str_detect(dataMatrix[i,j], regex)) {
				idMatrix[i,1] <- i
				idMatrix[i,2] <- j
			}
		}
	}

	idMatrix <- optimizeMatrix(idMatrix)
	print(idMatrix)
	return(idMatrix)
}

# funkcija optimizira matriko
# vrne samo vrednosti, ki so večje od nič
optimizeMatrix <- \(matrika) {
	firstRowChanged <- FALSE
	novaMatrika <- matrix(1,1,2)

	for (i in 1:nrow(matrika)) {
	    if (matrika[i,1] > 0 & matrika[i,2] > 0) {
			if (!firstRowChanged) {
				novaMatrika[1,] <- matrika[i,]
				firstRowChanged <- TRUE
			} else {
				novaMatrika <- rbind(novaMatrika, matrika[i,])
			}
		}
	}

	return(novaMatrika)
}

# funkcija dobi število ob znaku
# line je vrstica v matriki
getNumber <- \(line) {
	st <- regmatches(x=line, m=gregexpr(pattern="\\d", text=line)) %>%
		unlist() %>%
		paste0(collapse="") %>%
		as.numeric()

	ifelse(is.na(st), return(0), return(st))
}

# funkcija vrne za koliko mest nazaj ali naprej lahko gledamo
# če smo v 2. stolpcu, lahko gledamo nazaj samo 1 znak npr.
# dobimo vrednost med 0 in 3, ker so največ trimestna števila
# smer je bodisi "L"-levo, bodisi "D"-desno
getPremik <- \(matrika, colNum, smer) {
	maxCol <- ncol(matrika)

	premik <- case_when(
		smer == "L" ~ maxCol - (ncol(matrika[,colNum:maxCol])),
		smer == "D" ~ maxCol - (ncol(matrika[,1:colNum])),
		.default = 0
	)

	ifelse(premik>=3, return(3), return(premik))
}

# funkcija preveri, če je vodoravno levo od simbola število
# simbolIndeks je vrstica v matriki indeksov:
# prvi col je row, drugi col je col simbola
checkHorizontalLeft <- \(matrika, simbolIndeks) {
	row <- simbolIndeks[1]
	col <- simbolIndeks[2]
	premik <- getPremik(matrika=matrika, colNum=col, smer="L")

	if (premik == 0) {
		return(0)
	}

	prviCol <- col - premik
	drugiCol <- col - 1
	st <- getNumber(line = matrika[row,prviCol:drugiCol])

	return(st)
}

# funkcija preveri, če je vodoravno desno od simbola število
# simbolIndeks je vrstica v matriki indeksov:
# prvi col je row, drugi col je col simbola
checkHorizontalRight <- \(matrika, simbolIndeks) {
	row <- simbolIndeks[1]
	col <- simbolIndeks[2]
	premik <- getPremik(matrika=matrika, colNum=col, smer="D")

	if (premik == 0) {
		return(0)
	}

	prviCol <- col + 1
	drugiCol <- ncol(matrika)
	st <- getNumber(line = matrika[row,prviCol:drugiCol])

	return(st)
}

# funkcija preveri, če je vertikalno od simbola število
checkVertical <- \() {

}

# funkcija preveri, če je na diagonali spodaj levo število
checkBottomLeftDiagonal <- \() {

}

# funkcija preveri, če je na diagonali spodaj desno število
checkBottomRightDiagonal <- \() {

}

# funkcija preveri, če je na diagonali zgoraj levo število
checkTopLeftDiagonal <- \() {

}

# funkcija preveri, če je na diagonali zgoraj desno število
checkTopRightDiagonal <- \() {

}

