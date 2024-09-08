library(tidyverse)
library(readr)

# funkcije:
source("cubeFunctions.R", echo=T)

df <- parseFileData("data.txt")

# torej..
# kaj hočemo sploh vedet
# katere igre bi bile možne z 12 rdečimi, 13 zelenimi in 14 modrimi kockami?

# vsak row je en game
# 3 columns skupaj so ena runda
# vsako rundo povleče naključne kocke iz vreče
# po treh posegih v vrečo, jih vrne nazaj in začne novo rundo
# torej po 3 cols so ena runda

# moram pa vzeti tri stolpce naenkrat
colnames(df) %>% length()
# moram preveriti 18 stolpcev
# vsako rundo se kocke vrnejo
# vsaka runda ima 3 sete
# vsak set ima svojo vsoto kock

# vektor hrani katere igre so v vredu (part I)
okIgre <- seq(1:100); okIgre

# gremo čez vsak game in vsak set v game-u
# ko najdemo nonvalid game, ga odstranimo iz seznama
for (game in 1:nrow(df)) {
  okGame <- TRUE
  i <- 1
  
  while (i < ncol(df)) {
    # part I + dol
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

### DRUGI DEL ###
# koliko je bilo najmanjše število kock vsake barve, da je bila igra možna
# npr. game 1: 3 blue, 4 red; 1 red, 2 green, 6 blua; 2 green
# - min red: 4
# - min blue: 6
# - min green: 2
# Najmanjša moč seta je enaka zmnožku zgornjih vrednosti:
#   4*6*2=48
# vrniti moram vsoto vseh min moči setov

# torej gledam za vse igre, ne glede na to, ali je bila možna.
# problem je, ker ne morem gledat v istem while loopu, ker bo skippal nekatere sete
# moram narediti še en for in while loop

# potrebujem matriko, ki bo hranila trenutne max vrednosti
# 3 stolpci, 100 vrstic, same ničle

# vsako iteracijo for loopa bomo pisali v en row matrike
# v row shranimo števila, ki so rezultat funkcije getMinSetPower
setPowers <- matrix(0,nrow(df),3)
setPowers

for (game in 1:nrow(df)) {
  i <- 1
  
  while (i < ncol(df)) {
    cols <- c(i, i+2)
    kocke <- parseCubeData(col = df[game, cols[1]:cols[2]])

    setPowers[game,] %>%
      getMinSetPower(., kocke) -> setPowers[game,]

    i <- i+3
  }
  
}

setPowers

# moramo izračunati še zmonžek vseh stolpcev in vsoto vseh vrstic
# zmnožimo stolpce med sabo
# seštejemo vrstice
{setPowers[,1] * setPowers[,2] * setPowers[,3]} %>%
    sum() -> outPart2

write.table(outPart2, "answer2.txt", row.names = F, col.names = F)
