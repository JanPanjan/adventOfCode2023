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

#' data parse za day 6 podatke
parseData <- \(file) {
    time <- extractNums(file[1])
    distance <- extractNums(file[2])
    df <- data.frame(time = time, dst = distance)

    print(df)
    return(df)
}
