input <- read.table("~/workspace-private/adventofcode/adventofcode2024/input01.txt")
input <- read.table("~/workspace-private/adventofcode/adventofcode2024/input01-test.txt")

# part1
sum(abs(sort(input[,1]) - sort(input[,2])))

# part2
sum(input[,1] * table(input[,2])[as.character(input[,1])], na.rm = TRUE)