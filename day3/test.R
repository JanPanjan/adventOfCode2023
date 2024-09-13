# min test data
data_1 <- c("a", "b", "c", "d", "e", "3", "5", "*", "g", "h", "i", "j")
mat_1 <- matrix(data_1, 3, 4, byrow = T)

# real test data
file <- readFile("testData.txt")
mat_1 <- makeDataMatrix(file)
