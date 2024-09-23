library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

# file <- readFile("testData.txt")
file <- readFile("data.txt")
file <- readFile("testData.txt")

# dobimo data frame za vsak map
parseData <- \(file) {
    # funkcije so v "util.R"
    dobiMaps(file) -> maps
    dobiMapVrednosti(maps, file) -> maps_values
    dobiDataFrames(maps_values) -> dfs
    return(dfs)
}

dfs <- parseData(file)
dfs

# dobimo semena
seeds <- file[1] %>%
    sub(".*: ", "", .) %>%
    str_split_1(" ") %>%
    as.numeric()
seeds

# imamo 20 števil
# liha je start, soda je len intervala semen

# namesto enega števila, preslikamo range števil
# moramo ugotoviti, katera semena so znotraj source intervala

# seedRange je nek interval seeds
# preslikali bomo cel range namesto posameznih števil

# hočem, da vzamemo en seed range in ga modificiramo
# in nato naslednji seed range, itd.
dobiNoveVrednosti <- \(df, seedRange) {
    newSeeds <- c() # vektor, ki bo nosil modificirane vrednosti

    # BUG: nimam pojma, če lahko dvakrat modificiramo isto število
    # če JA, potem moramo keepat track tistih, ki smo jih že dali skozi...

    # dobimo nove vrednosti ali vrnemo seed
    # gremo čez vrstice od posamezne map
    for (j in 1:nrow(df)) {
        dst_start <- df$dst[j] %>% as.numeric()
        src_start <- df$src[j] %>% as.numeric()
        range <- df$range[j] %>% as.numeric()

        # dobimo source range, ki predstavlja valid seeds
        src_range <- seq(src_start, src_start + range - 1)

        # semena, ki so valid spremenimo
        # ostala pustimo taka kot so
        ids <- which(seedRange %in% src_range)

        # če ni noben seed vredu, vrnemo preden modificiramo
        if (length(ids) == 0) {
            newSeeds <- c(newSeeds, seedRange)
            next
        }
        # na ids pokličemo funkcijo, ki preslika vrednosti
        preslikava <- \(x) {
            return(dst_start + (x - src_start))
        }
        newSeeds <- c(newSeeds, unlist(sapply(seedRange[ids], preslikava)))
    }
    return(newSeeds)
}

# od zgornje funkcije dobimo modificirane vrednosti za vsa semena
# ampak samo za en df
# zato moramo poslati vrednosti v pipe skozi vsak df
# dokler ne dobimo location vrednosti

# pač, mi pretvorimo vse range naenkrat in vrnemo vektor vseh
# uporabimo dani df, zato moramo zdaj pošiljati modificirane vrednosti v vsak df
dobiLocationValues <- \(dfs, seeds) {
    # vektor bo hrnil location values
    location_values <- c()

    for (i in seq(1, length(seeds), 2)) {
        seedRange <- seq(seeds[i], seeds[i] + seeds[(i + 1)] - 1)

        dobiNoveVrednosti(dfs[[1]], seedRange) %>%
            dobiNoveVrednosti(dfs[[2]], .) %>%
            dobiNoveVrednosti(dfs[[3]], .) %>%
            dobiNoveVrednosti(dfs[[4]], .) %>%
            dobiNoveVrednosti(dfs[[5]], .) %>%
            dobiNoveVrednosti(dfs[[6]], .) %>%
            dobiNoveVrednosti(dfs[[7]], .) -> newVals

        location_values <- c(location_values, newVals)
    }

    return(location_values)
}

# dobimo vrednosti
dobiLocationValues(dfs, seeds) -> LOCATIONS
LOCATIONS
min(LOCATIONS)

# vrnemo najmanjšo
writeText(min(LOCATIONS), "odgovor2test.txt")
# writeText(min(LOCATIONS), "odgovor1.txt")
