require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input08.txt")

d <- stri_split_fixed(input, "|", n=2, simplify=TRUE)
sum(table(stri_length(stri_split_fixed(d[,-1], " ", simplify = TRUE)[,-1]))[c('2', '3', '4', '7')])


e = stri_split_regex(input, " |\\|", omit_empty = TRUE)


find_digits <- function(x) {
  y <- stri_paste("[^", sapply(c(2,4,7), function(k) x[sapply(x, nchar) == k][1]), "]")
  tmp <- apply(sapply(y, function(z) nchar(gsub(z, "", x))), 1, function(z) sum(z * 3^(2:0)))
  map_vec <- c(33, 26, 20, 32, 34, 23, 24, 27, 37, 36)
  sum(sapply(tmp, function(k) which(map_vec == k) - 1) * 10^(3:0))
}

sum(sapply(e, find_digits))

find_digits(e[[1]])
