library(tidyverse)
library(readr)

# read file with data
df <- read.delim("data.txt", header = FALSE, sep = ";"); df

# make columns
df <- separate(data = df, col = V1, into = c("game", "V1"), sep = ":"); df
df %>% separate(V1, c("1-1","1-2","1-3"), sep = ",") -> df
df %>% separate(V2, c("2-1","2-2","2-3"), sep = ",") -> df
df %>% separate(V3, c("3-1","3-2","3-3"), sep = ",") -> df
df %>% separate(V4, c("4-1","4-2","4-3"), sep = ",") -> df
df %>% separate(V5, c("5-1","5-2","5-3"), sep = ",") -> df
df %>% separate(V6, c("6-1","6-2","6-3"), sep = ",") -> df;df

# game stolpec lahko odstranim v bistvu
df <- select(df, !game)

df %>% as_tibble()

# torej..
# kaj hočemo sploh vedet
# katere igre bi bile možne z 12 rdečimi, 13 zelenimi in 14 modrimi kockami?

# vsak row je en game
# 3 columns skupaj so ena runda
# vsako rundo povleče naključne kocke iz vreče
# po treh posegih v vrečo, jih vrne nazaj in začne novo rundo
# torej po 3 cols so ena runda

# moram najt skupni seštevek istih kock za vsako rundo
# naredim vektor velikost 3 (rdeča, modra, zelena)
# vrednosti moram gledati rowwise
# naredim funkcijo ki bo parsala vrednost
#  - potrebujem število
#  - potrebujem barvo
#  - glede na barvo, pripišem vrednost v vektor
#  - če je NA, ni težav
# naredim to s celim stolpcem
# naredim to z vsemi stolpci
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

# moram pa vzeti tri stolpce naenkrat
colnames(df) %>% length()
# moram preveriti 18 stolpcev
# vsako rundo se kocke vrnejo
# vsaka runda ima 3 sete
# vsak set ima svojo vsoto kock

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

# vektor hrani katere igre so v vredu
okIgre <- seq(1:100); okIgre

# gremo čez vsak game in vsak set v game-u
# ko najdemo nonvalid game, ga odstranimo iz seznama
for (game in 1:100) {
  okGame <- TRUE
  i <- 1
  
  while (i < 19) {
    cols <- c(i, i+2)
    kocke <- parseCubeData(col = df[game, cols[1]:cols[2]])
    i <- i+3
    
    if (dosezenLimit(kocke)) {
      print(paste("Game:", game, "dosežen limit."))
      okGame <- FALSE
      break
    }
  }

  if (!okGame) {
    okIgre[game] <- 0
  }
}

# sum okIgre je naša rešitev
sum(okIgre) -> out
write.table(out, "odgovor.txt", row.names = F, col.names = F)

