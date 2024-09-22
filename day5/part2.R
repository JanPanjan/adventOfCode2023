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
dobiNoveVrednosti <- \(df, seeds) {
    newSeeds <- c()

    for (i in seq(1, length(seeds), 2)) {
        # dobimo seed range
        seedRange <- seq(seeds[i], seeds[i] + seeds[(i+1)] - 1)

        # dobimo nove vrednosti ali vrnemo seed
        for (j in 1:nrow(df)) {
            dst_start <- df$dst[j] %>% as.numeric()
            src_start <- df$src[j] %>% as.numeric()
            range <- df$range[j] %>% as.numeric()

            # dobimo source range
            src_range <- src_start:(src_start + range - 1)

            # semena, ki so v range spremenimo
            # ostala pustimo taka kot so
            ids <- which(seedRange %in% src_range)
            # če ni noben seed vredu, vrnemo preden modificiramo
            if (length(ids) == 0) {
                newSeeds <- c(newSeeds, seedRange)
            }

            # na ids pokličemo funkcijo, ki preslika vrednosti
            preslikava <- \(x) {
                return(dst_start + (x - src_start))
            }

            modSeedRange <- sapply(seedRange[ids], preslikava) %>% unlist()

            # vrnemo modificiran seedRange
            newSeeds <- c(newSeeds, modSeedRange)
        }
    }

    return(newSeeds)
}

# funkcija dobi location values za vsako seme, tako da se sprehodi čez maps
# vrne vektor števil
dobiLocationValues <- \(dfs, seeds) {
    location_values <- c()

    for (i in 1:length(seeds)) {
        seed <- seeds[i]
        # naredimo pipe
        dobiNoveVrednosti(dfs[[1]], seed) %>%
            dobiNoveVrednosti(dfs[[2]], .) %>%
            dobiNoveVrednosti(dfs[[3]], .) %>%
            dobiNoveVrednosti(dfs[[4]], .) %>%
            dobiNoveVrednosti(dfs[[5]], .) %>%
            dobiNoveVrednosti(dfs[[6]], .) %>%
            dobiNoveVrednosti(dfs[[7]], .) -> newVal

        location_values <- c(location_values, newVal)
    }

    return(location_values)
}

# dobimo vrednosti
dobiLocationValues(dfs, noviSeeds) -> LOCATIONS
LOCATIONS
min(LOCATIONS)

# vrnemo najmanjšo
writeText(min(LOCATIONS), "odgovor2test.txt")
# writeText(min(LOCATIONS), "odgovor1.txt")
