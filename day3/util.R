library(tidyverse)
# reads text file by filename
# returns vector
readFile <- \(filename) {
	file <- read.delim(filename, header=F) |>
		unlist() |>
		setNames(NULL)
	
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

	return(mat)
}

# makes data frame by spliting every character in vec
makeDf <- \(vec) {
	listData <- strsplit(vec, split="")
	len <- length(listData)
	df <- data.frame()

	for (i in 1:len) {
		for (j in 1:len) {
			df[i,j] <- listData[[i]][j]
		}
	}

	return(df)
}

# funkcija vrne matriko indeksov od vseh validnih simbolov
getSymbols <- \(dataMatrix) {
	regex <- c("[#|$|%|&|/|=|?|*|+|-|@]")
	rows  <- nrow(dataMatrix)
	cols  <- ncol(dataMatrix)
	idMatrix <- matrix(0,2*rows,2)

	for (i in 1:rows) {
		for (j in 1:cols) {
			if (str_detect(dataMatrix[i,j], regex)) {
				print(paste(i,j))
				idMatrix[i,1] <- i
				idMatrix[i,2] <- j
			}
		}
	}

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

# funkcija preveri, če je vodoravno od simbola število
checkHorizontal <- \() {

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

# funkcija dobi število ob znaku
getNumber <- \() {

}
