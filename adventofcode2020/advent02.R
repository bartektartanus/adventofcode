
input <- readLines("~/workspace-private/adventofcode2020/input02.txt")

require(stringi)

a <- stri_match_first_regex(input, "^(\\d+)-(\\d+) ([a-z]): (.*)$")

# part 1
c <- stri_count_fixed(a[,5], a[,4])
sum(c >= as.integer(a[,2]) & c <= as.integer(a[,3]))

# part 2
sum(xor(stri_sub(a[,5], a[,2], len = 1) == a[,4], stri_sub(a[,5], a[,3], len = 1) == a[,4]))
