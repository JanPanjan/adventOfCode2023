library(tidyverse)
source("C:/Users/joene/Documents/progAAAAAAA/adventOfCode2023/util.R", chdir = TRUE, echo = TRUE)

file <- readFile("testData.txt")
file <- readFile("data.txt")

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

# v listu imamo maps, torej bomo morali skozi vsak df v listu
# prvo podano število je seed number
# ko predelamo en df, dobimo število, ki ga podamo v naslednji df
# na koncu nam vrne location number - to kar iščemo

# funkcija vzame posamezen df iz dfs in seed
# vrne novo vrednost
dobiNovoVrednost <- \(df, seed) {
    for (i in 1:nrow(df)) {
        src_start <- df$src[i] %>% as.numeric()
        dst_start <- df$dst[i] %>% as.numeric()
        range <- df$range[i] %>% as.numeric()

        # če je seed v src_intervalu, dobimo novo vrednost glede na dst_interval
        # if (seed %in% names(src_interval)) {
        if (seed >= src_start && seed < (src_start + range)) {
            print(paste("seed", seed, "in interval", src_start, src_start + range))
            new_val <- dst_start + (seed - src_start)
            print(paste("new value", new_val))

            return(new_val)
        }
    }

    # če ni, vrnemo seed
    print("seed not in interval.")
    return(seed)
}

# funkcija dobi location values za vsako seme, tako da se sprehodi čez maps
# vrne vektor števil
dobiLocationValues <- \(dfs, seeds) {
    location_values <- c()

    for (i in 1:length(seeds)) {
        seed <- seeds[i]
        # naredimo pipe
        dobiNovoVrednost(dfs[[1]], seed) %>%
            dobiNovoVrednost(dfs[[2]], .) %>%
            dobiNovoVrednost(dfs[[3]], .) %>%
            dobiNovoVrednost(dfs[[4]], .) %>%
            dobiNovoVrednost(dfs[[5]], .) %>%
            dobiNovoVrednost(dfs[[6]], .) %>%
            dobiNovoVrednost(dfs[[7]], .) -> newVal

        location_values <- c(location_values, newVal)
    }

    return(location_values)
}

# dobimo vrednosti
dobiLocationValues(dfs, seeds) -> LOCATIONS
LOCATIONS

# vrnemo najmanjšo
writeText(min(LOCATIONS), "odgovor1test.txt")
writeText(min(LOCATIONS), "odgovor1.txt")
