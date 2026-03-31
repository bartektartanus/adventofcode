require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input09.txt")
testInput = stri_split_fixed("0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45","\n")[[1]]


extrapolate = function(history, reverse = FALSE) {
  h = as.integer(stri_split_fixed(history, " ")[[1]])
  if(reverse) {
    h = rev(h)
  }
  l = list()
  ch = h
  while(!all(ch == 0)) {
    l[[length(l) + 1]] = ch
    ch = diff(ch)
  }
  last = 0
  for(i in rev(seq_along(l))) {
    last = tail(l[[i]], 1) + last
  }
  last
}

part1 = function(x) {
  sum(sapply(x, extrapolate))
}

part1(testInput) == 114
part1(input)

part2 = function(x) {
  sum(sapply(x, function(y) extrapolate(y, TRUE)))
}

part2(testInput) == 2
part2(input)
