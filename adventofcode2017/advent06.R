require("stringi")
input <- as.integer(read.table("input06.txt", sep="\t")[1,])
inputTest <- c(0,2,7,0)

states = function(input) {
  s = stri_join(input, collapse = ",")
  n = length(input)
  while(!any(duplicated(s))) {
    m = which.max(input)
    v = input[m]
    input[m] = 0
    for(i in seq_len(v)) {
      input[(m+i-1) %% n + 1] = input[(m+i-1) %% n + 1] + 1
    }
    s = c(s, stri_join(input, collapse = ","))
  }
  s
}

part1 = function(input) {
  length(states(input)) - 1
}

part1(inputTest) == 5
part1(input)

part2 = function(input) {
  s = states(input)
  i = which(duplicated(s))
  diff(which(s == s[i]))
}

part2(inputTest) == 4
part2(input)

