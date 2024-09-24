# Part 1

input: 79, 14, 55, 13
length je 4 (sodo)

output: 82, 43, 86, 35

## 1. gremo čez vsak range semen

naj bo [seed_range] <- [input[i]:input[i]+input[i+1]]

### source range

[src_range]: nek interval [a:a+d], kjer [a] = start in [d] = length

Potrebujemo samo meje od source range:
- naj bo [src_levo] <- a
- naj bo [src_desno] <- a+d

## 2. preverimo katera so znotraj src_range mej

najdemo semena so znotraj [a] in [a+d]
naj bo [leva] tisti seed, ki je večji ali enak [a]
naj bo [desna] tista, ki je manjša [a+d]

## 3. modificiramo range med [leva:desna]

### funkcija [mod_seeds]

input: range [leva:desna]
return: nove vrednosti semen

### destination range

[dst_range]: nek interval [b:b+d], kjer [b] = start in [d] = length

Potrebujemo samo start od dst_range - [b]

Število spremenimo tako, da 

## 4. vrnemo range semen

