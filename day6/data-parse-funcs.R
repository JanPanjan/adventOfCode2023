#' extract numbers
#'
#' @description
#' funkcija dobi števila iz stringa
#' parse_number v readr vrne samo prvo številko
#' ta funkcija vrne vsa števila
#'
#' @param str_vec string vektor s števili
#' @returns numeric vektor
extractNums <- \(str_vec) {
    out <- sub(".*:", "", str_vec) %>%
        strsplit(" ") %>%
        unlist() %>%
        grep(pattern = "\\d", value = T) %>%
        as.numeric()

    if (length(out) == 0) {
        message("no numbers in vector.")
        return(NA)
    }

    return(out)
}

extractNum <- \(str_vec) {
    out <- sub(".*:", "", str_vec) %>%
        gsub("\\s+", " ", .) %>%
        strsplit(" ") %>%
        unlist() %>%
        paste0(collapse = "") %>%
        as.numeric()

    return(out)
}

#' data parse za day 6 part 1 podatke
#'
#' @param file string vector
parseData <- \(file) {
    time <- extractNums(file[1])
    distance <- extractNums(file[2])
    df <- data.frame(time = time, dst = distance)

    print(df)
    return(df)
}

#' data parse za day 6 part 2 podatke
#'
#' @param file string vector
parseDataAgain <- \(file) {
    data <- list(
        time = extractNum(file[1]),
        distance = extractNum(file[2])
    )

    print(data)
    return(data)
}
