require(stringi)
input <- sort(as.integer(readLines("~/workspace-private/adventofcode/adventofcode2015/input17.txt")),decreasing = TRUE)
inputTest <- c(20, 15, 10, 5, 5)


f = function(input, total = 150) {
  r = matrix(NA, total, length(input))
  
  
  sum(r[total,], na.rm = TRUE)
}

f(inputTest, 50)
f(input)