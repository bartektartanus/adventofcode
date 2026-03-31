require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input11.txt")
testInput = stri_split_fixed("...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....","\n")[[1]]

part1 = function(x) {
  m = t(sapply(x, function(y) stri_sub(y, 1:stri_length(y), len=1), simplify = "array"))
  r = rowSums(m == ".") == nrow(m)
  c = colSums(m == ".") == ncol(m)
  e = m
  e = e[sort(c(1:nrow(e), which(r))),]
  e = e[,sort(c(1:ncol(e), which(c)))]
  g = which(e == "#", arr.ind = TRUE)
  s = 0
  for(i in 1:nrow(g)) {
    for(j in i:nrow(g)){
      s = s + sum(abs(g[i,] - g[j,]))
    }
  }
  s
}

part1(testInput) == 374
part1(input)

part2 = function(x, expand = 2) {
  m = t(sapply(x, function(y) stri_sub(y, 1:stri_length(y), len=1), simplify = "array"))
  r = which(rowSums(m == ".") == nrow(m))
  c = which(colSums(m == ".") == ncol(m))
  g = which(m == "#", arr.ind = TRUE)
  s = 0
  for(i in 1:nrow(g)) {
    for(j in i:nrow(g)){
      dr = sum(min(g[i,1], g[j,1]) < r & max(g[i,1], g[j,1]) > r)
      dc = sum(min(g[i,2], g[j,2]) < c & max(g[i,2], g[j,2]) > c)
      d = sum(abs(g[i,] - g[j,])) + (dr+dc) * (expand-1)
      s = s + d
    }
  }
  s
}

part2(testInput) == 374
part2(testInput, 10) == 1030
part2(testInput, 100) == 8410
part2(input)
part2(input, 1000000)
