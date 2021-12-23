input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input10.txt')
input <- c(16,10,
           15,
           5,
           1,
           11,
           7,
           19,
           6,
           12,
           4)

require(stringi)

input <- as.integer(input)
input <- sort(c(input, 0, max(input)+3))

sum(diff(input)==1)*sum(diff(input)==3)
diff(sort(input))


x <- unique(input[c(1, which(diff(input) == 3), which(diff(input) == 3) + 1, length(input))])
x
print(prod(pmin(2^table(cumsum(c(0, diff(setdiff(input, x)) > 2))), 7)), digits=22)
