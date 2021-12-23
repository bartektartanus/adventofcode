input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input01.txt")
input <- as.integer(input)

sum(diff(input) > 0)

input2 <- input[1:1998] + input[2:1999] + input[3:2000]
sum(diff(input2) > 0)
