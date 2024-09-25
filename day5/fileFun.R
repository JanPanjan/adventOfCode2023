# dobimo data frame za vsak map
parseData <- \(file) {
    # funkcije so v "util.R"
    dobiMaps(file) -> maps
    dobiMapVrednosti(maps, file) -> maps_values
    dobiDataFrames(maps_values) -> dfs
    return(dfs)
}

imena <- c("seed-soil", "soil-fert", "fert-water", "water-light", "light-temp", "temp-hum", "hum-loc")
# funkcija vrne vektor indeksov, kjer so imena od maps (npr. "seed-to-soil map:")
# seeds ([1]) se ne shrani, ker filtriramo glede na "\\d"
dobiMaps <- \(file) {
    # moramo dobit vsak map iz podatkov
    # nimamo praznih vrstic, moramo prebrati vrstice, dokler ne najdemo naslednji map
    maps <- map(file, grepl, pattern = "\\d") %>%
        unlist() %>%
        setNames(., c(1:length(.))) %>%
        grep(x = ., pattern = FALSE) %>%
        setNames(., imena)

    return(maps)
}

# vzamemo od indeksa v vektorju, do naslednjega indexa v vektorju
# če je zadnji indeks, vzamemo do konca

# funkcija vrne naše vrednosti od maps (integerje)
# destination, source in range
# vsak map ima lahko več takih intervalov
# vrne list, kjer je vsak element en map in pod-elementi so ti intervali
dobiMapVrednosti <- \(maps, file) {
    # vzamemo od indeksa v vektorju, do naslednjega indexa v vektorju
    # če je zadnji indeks, vzamemo do konca
    intervali <- list()

    for (i in seq(1, length(maps))) {
        if (i == length(maps)) {
            intervali[[i]] <- file[(maps[i] + 1):length(file)]
        } else {
            intervali[[i]] <- file[(maps[i] + 1):(maps[i + 1] - 1)]
        }
    }

    names(intervali) <- imena
    return(intervali)
}

# funkcija vrne data frame iz vrednosti elementa v listu
# naredi tri stolpce: dst, src in range za map
narediDataFrame <- \(map, name) {
    df <- data.frame(ena = unlist(map)) %>%
        {
            row.names(.) <- NULL
            .
        } %>%
        separate(col = ena, into = c("dst", "src", "range"), sep = " ")

    return(df)
}

# funkcija naredi list z data frames, ki so naši maps
dobiDataFrames <- \(maps_values) {
    dfs <- list()

    for (i in 1:length(names(maps_values))) {
        dfs[[i]] <- narediDataFrame(maps_values[[i]], names(maps_values)[i])
    }

    names(dfs) <- imena
    return(dfs)
}
