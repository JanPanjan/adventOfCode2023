library(tidyverse)

# dobimo podatke
file <- read.delim("data.txt", header=F) %>% 
	unlist() %>% 
	setNames(NULL)

# moram naredit grep za vsa števila od 1 do 9 (črkovno)
# najprej moram najt prvo in zadnjo številko

# pogledamo če najdemo katero vrednost iz `stevila`
# dvojni for loop, ki gradi besede

# max length besede je 5, right
# vzamemo i in gledamo 4 crke naprej
# vsak j gledamo če je alpha ali digit stevilo
# če je se shrani v nums[1]
# gledamo za drugo stevilo; ko najdemo, break, drugace gre do konca
# če po koncu ni drugega števila, vzamemo dvakrat prvo

# spet moramo sešet vse vrednosti

getCalibrationData <- \(line) {
	nums <- c(0,0)
	stevila <- c( one=1, two=2, three=3, four=4, five=5, six=6, seven=7, eight=8, nine=9)

	for (i in 1:length(line)) {
		print(nums)

		for (j in 1:length(line)) {
			# gledamo, če je prvi znak v trenutni besedi število
			if (i == j) {
				znak <- as.numeric(line[i:j])

				if (znak %in% stevila) {
					# če je prvi element prost, shranimo prvo število
					# drugače se vsako naslednje shrani v drugo število
					if (nums[1] == 0) {
						nums[1] <- znak
					} else {
						nums[2] <- znak
					}
				}
			}
			if (i <= j) {
				word <- paste0(line[i:j], collapse="")

				# gledamo, če nastane število iz črk
				if (word %in% names(stevila)) {
					znak <- stevila[word]

					if (nums[1] == 0) {
						nums[1] <- znak
					} else {
						nums[2] <- znak
					}
				}
			}
		}
	}

	# če je našlo samo eno število, moramo narediti dvomestno iz enega
	if (nums[2] == 0) {
		nums[2] <- nums[1]
	}

	return(as.numeric(paste0(nums, collapse="")))
}

t <- str_split_1("eightwo1three", ""); t
t <- str_split_1("8enadvatri", ""); t
getCalibrationData(t) -> calibrationData; calibrationData

# zdaj moramo to narediti za vse vrstice
sapply(file, str_split, pattern="") -> splited; splited

# LONG PROCESS
# imagine, da bi mel 100000 vrstic...
map(splited, getCalibrationData) -> data; data

# :D
# še seštejemo vse vrednosti v listu
sum(unlist(data)) -> CALIBRATION_DATA; CALIBRATION_DATA

write.table(CALIBRATION_DATA, "odgovor2.txt", row.names=F, col.names=F)


# yeah, vzame veliko časa, ampak končno dela
