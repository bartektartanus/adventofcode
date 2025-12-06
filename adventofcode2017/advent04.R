require("stringi")
input <- readLines("input04.txt")

part1 = function(input) {
  sum(!sapply(stri_split_fixed(input, " "), function(x) any(duplicated(x))))
}

part1(input)

sortLetters = function(x) {
  sapply(x, function(x) stri_paste(sort(stri_sub(x, 1:stri_length(x), len=1)),collapse=""))
}

part2 = function(input) {
  sum(!sapply(stri_split_fixed(input, " "), function(x) any(duplicated(sortLetters(x)))))
}


part2(input)

