require(stringi)
require(numbers)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input08.txt")
testInput = stri_split_fixed(c("RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"), "\n")[[1]]

part1 = function(x, start = "AAA", end = "ZZZ") {
  steps = stri_sub(x[1], 1:stri_length(x[1]), len=1)
  x = x[-(1:2)]
  f = stri_extract_all_regex(x, "[A-Z\\d]{3}", simplify = TRUE)
  f = data.frame(L=f[,2], R=f[,3], row.names=f[,1])
  e = start
  i = 0
  while(!any(e == end)) {
    i = i + 1
    s = steps[(i-1) %% length(steps) + 1]
    e = f[e, s]
  }
  i
}

part1(testInput)
part1(input)

part2 = function(x) {
  steps = stri_sub(x[1], 1:stri_length(x[1]), len=1)
  f = stri_extract_all_regex(x[-(1:2)], "[A-Z\\d]{3}", simplify = TRUE)
  nodes = f[,1]
  start = nodes[stri_endswith_fixed(nodes, "A")]
  end = nodes[stri_endswith_fixed(nodes, "Z")]
  format(mLCM(sapply(start, function(y) part1(x, y, end))), digits = 22)
}
testInput2 = stri_split_fixed(c("LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"), "\n")[[1]]
part2(testInput2)
part2(input)
