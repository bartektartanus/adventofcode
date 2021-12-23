require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input07.txt")
input = as.integer(stri_split_fixed(input,",")[[1]])

mean(input)

sum(abs(median(input)-input))

fuelCost = function(input, x) {
  sum(abs(x-input))
}
minCost = fuelCost(input, input[1])
for(i in min(input):max(input)) {
  minCost = min(fuelCost(input, i), minCost)
}
minCost

cost = cumsum(1:diff(range(input)))
fuelCost2 = function(input, x) {
  sum(cost[abs(x-input)])
}

minCost = fuelCost2(input, input[1])
for(i in min(input):max(input)) {
  minCost = min(fuelCost2(input, i), minCost)
}
minCost
