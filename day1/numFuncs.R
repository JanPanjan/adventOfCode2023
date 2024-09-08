stevila <- c(
	one=1, two=2, three=3, four=4, five=5, six=6, seven=7, eight=8, nine=9
)

# pogleda če je x število
# če ni, vrne 0
# če je, vrne število
getDigit <- \(x) {
	if (is.character(x) | is.na(x)) {
		return(0)
	} else {
		return(x)
	}
}

shraniNums <- \(word, nums) {
	if(nums[1] == 0) {
		nums[1] <- stevila[word]
	} else {
		nums[2] <- stevila[word]
	}
}
