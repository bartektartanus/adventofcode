require("stringi")
input <- readLines("input05.txt")
inputTest = stri_split_lines("3-5
10-14
16-20
12-18

1
5
8
11
17
32")[[1]]

part1 = function(input) {
  e = which(input == "")
  rangeList = matrix(as.numeric(stri_split_fixed(head(input, e-1), "-", n = 2, simplify = TRUE)), ncol=2)
  ingredientList = as.numeric(tail(input, length(input) - e))
  s = 0
  for(ingredient in ingredientList) {
    for(r in seq_len(nrow(rangeList))) {
      if(ingredient >= rangeList[r,1] && ingredient <= rangeList[r,2]) {
        s = s+1
        break;
      }
    }
  }
  s
}

part1(inputTest)
part1(input)

extendRange = function(rangeList) {
  for(i in 1:(nrow(rangeList))) {
    for(j in 1:nrow(rangeList)) {
      if(rangeList[i,1] >= rangeList[j,1] && rangeList[i,1] <= rangeList[j,2]) {
        rangeList[i,1] = rangeList[j,1]
      }
      if(rangeList[i,2] >= rangeList[j,1] && rangeList[i,2] <= rangeList[j,2]) {
        rangeList[i,2] = rangeList[j,2]
      }
    }
  }
  rangeList
}

part2 = function(input) {
  e = which(input == "")
  rangeList = matrix(as.numeric(stri_split_fixed(head(input, e-1), "-", n = 2, simplify = TRUE)), ncol=2)
  while(TRUE) {
    rangeList = extendRange(rangeList)
    d = !duplicated(stri_paste(rangeList[,1],"-", rangeList[,2]))
    if(all(d)) {
      break;
    }
    rangeList = rangeList[d,]
  }
  sum(rangeList[,2] - rangeList[,1] + 1)
}

part2(inputTest)
format(part2(input), digits=22)

