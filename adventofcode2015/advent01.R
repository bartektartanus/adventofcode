input <- readLines("~/workspace-private/adventofcode2015/input01.txt")

require(stringi)

# part1
sum(stri_count_fixed(input, c('(', ')')) * c(1,-1))

# part2
x <- stri_extract_all_regex(input, '.')[[1]]
y <- rep(1, length(x))
y[x == ')'] <- -1
which(cumsum(y) == -1)
