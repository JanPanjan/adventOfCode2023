library(tidyverse)

# funkcije
source("numFuncs.R")

# dobimo podatke
file <- read.delim("test.txt", header=F) %>% 
	unlist() %>% 
	setNames(NULL); file

# moram naredit grep za vsa števila od 1 do 9 (črkovno)
# najprej moram najt prvo in zadnjo številko

# pogledamo če najdemo katero vrednost iz `stevila`
# dvojni for loop, ki gradi besede

# max length besede je 5, right
map(stevilaAlpha, nchar) %>% unlist() %>% max()
# yep

# vzamemo i in gledamo 4 crke naprej
# vsak j gledamo če je alpha ali digit stevilo
# če je se shrani v nums[1]
# gledamo za drugo stevilo; ko najdemo, break, drugace gre do konca
# če po koncu ni drugega števila, vzamemo dvakrat prvo

# spet moramo sešet vse vrednosti
calibrationData <- 0

# primer: "two1nine"
test <- file[2] %>% str_split_1(""); test
nums <- c(0,0)

for (i in 1:length(test)) {
	# hrani besedo, max len 5
	# vsak nov i, se ponastavi
	wordNum <- rep("", 5)

    for (j in i:min(length(test), i + 4)) {
		# 1:1, 1:2, 1:3...
		# 2:2, 2:3, 2:4...
		# da lahko nadaljujemo po nizu
		print(paste("i:",i," ","j:",j))
		if (i <= j) {
			wordNum[j] <- test[j]
			# gledamo, če nastane število iz črk
			word <- paste0(wordNum, collapse="")
			print(word)

			if (word %in% names(stevila)) {
				print("JE V STEVILIH")
				# če je prvi element prost, shranimo prvo število
				# drugače se vsako naslednje shrani v drugo število
				if (nums[1] == 0) {
					nums[1] <- stevila[word]
				} else {
					nums[2] <- stevila[word]
				}
			}
		}
		# gledamo, če je prvi znak v trenutni besedi število
		if (i == j) {
			znak <- test[i:j]

			if (as.numeric(znak) %in% stevila) {
				print("JE V STEVILIH")
				# če je prvi element prost, shranimo prvo število
				# drugače se vsako naslednje shrani v drugo število
			    if (nums[1] == 0) {
					nums[1] <- as.numeric(znak)
				} else {
					nums[2] <- as.numeric(znak)
				}
			}
			
		}
    }
}
nums

# dodamo število v vektor
calibrationData + as.numeric(paste0(nums, collapse=""))

paste0("", "", "e","n","a", collapse="")
