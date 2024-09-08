# v datoteki, kjer želiš te funkcije, pokliči:
# source("cubeFunctions.R")

# da mi ni treba vsakič znova klicat vseh vrstic
parseFileData <- \(filename) {
  df <- read.delim(filename, header = FALSE, sep = ";")

  # make columns
  df <- tidyr::separate(df, V1, c("game", "V1"), sep = ":")
  df <- tidyr::separate(df, V1, c("1-1","1-2","1-3"), sep = ",")
  df <- tidyr::separate(df, V2, c("2-1","2-2","2-3"), sep = ",")
  df <- tidyr::separate(df, V3, c("3-1","3-2","3-3"), sep = ",")
  df <- tidyr::separate(df, V4, c("4-1","4-2","4-3"), sep = ",")
  df <- tidyr::separate(df, V5, c("5-1","5-2","5-3"), sep = ",")
  df <- tidyr::separate(df, V6, c("6-1","6-2","6-3"), sep = ",")

  # game stolpec lahko odstranim v bistvu
  df <- dplyr::select(df, !game)
  df %>% tibble::as_tibble()
  return(df)
}

# koliko je bilo najmanjše število kock vsake barve, da je bila igra možna
# npr. game 1: 3 blue, 4 red; 1 red, 2 green, 6 blua; 2 green
# - min red: 4
# - min blue: 6
# - min green: 2
getMinSetPower <- \(currentMinValues, newValues) {
  for (i in 1:lenght(currentMinValues) {
    if (currentMinValues[i] < newValues[i]) {
      currentMinValues[i] <- newValues[i]
    }
  }

  return(currentMinValues)
}

# moram najt skupni seštevek istih kock za vsako rundo
# naredim vektor velikost 3 (rdeča, modra, zelena)
# vrednosti moram gledati rowwise
# naredim funkcijo ki bo parsala vrednost
#  - potrebujem število
#  - potrebujem barvo
#  - glede na barvo, pripišem vrednost v vektor
#  - če je NA, ni težav
parseCubeData <- \(col) {
  barve <- c(blue=0, green=0, red=0)
  col <- as.character(col)

  # vrne matriko
  str_split(col, " ", n=3, simplify = T) %>% 
    .[,-1] -> middle 
  
  for (i in 1:nrow(middle)) {
    barve[middle[i,2]] <- as.numeric(middle[i,1])
  }
  
  return(barve[1:3])
}

# za vsakim setom moramo preveriti, ali so vrednosti prekoračile limit
# modra: 14
# zelena: 13
# rdeča: 12
dosezenLimit <- \(kocke) {
  if (kocke["blue"] > 14 | kocke["green"] > 13 | kocke["red"] > 12) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
