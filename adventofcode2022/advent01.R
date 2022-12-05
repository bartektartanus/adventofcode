input = readLines("~/workspace-private/adventofcode/adventofcode2022/input01")

sum = 0
allSum = c()
for(x in input) {
  if(x == "") {
    allSum = c(allSum, sum)
    sum = 0
  } else {
    sum = sum + as.integer(x)
  }
}
max(allSum)

# part 2
sum(sort(allSum, decreasing = TRUE)[1:3])
