require("stringi")
input <- readLines("input08.txt")
inputTest = stri_split_lines("162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")[[1]]

part1 = function(input, iter = 10) {
  x = matrix(as.integer(stri_split_fixed(input, ",", n=3, simplify = TRUE)), ncol=3)
  d = as.matrix(dist(x))
  for(i in 1:nrow(d)) {
    d[i,i] = Inf
  }
  r = as.list(1:nrow(d))
  for(i in 1:iter) {
    m = which(d == min(d), arr.ind = TRUE)
    d[m] = Inf
    a = which(sapply(r, function(x) any(x == m[1,1])))
    b = which(sapply(r, function(x) any(x == m[1,2])))
    if(a!=b) {
      
    r[[a]] = c(r[[a]], r[[b]])
    r[[b]] = NULL
    }
  }
  prod(tail(sort(sapply(r, length)),3))
}

part1(inputTest, 10) == 40
part1(input, 1000)

part2 = function(input) {
  x = matrix(as.integer(stri_split_fixed(input, ",", n=3, simplify = TRUE)), ncol=3)
  d = as.matrix(dist(x))
  for(i in 1:nrow(d)) {
    d[i,i] = Inf
  }
  r = as.list(1:nrow(d))
  while(TRUE) {
    m = which(d == min(d), arr.ind = TRUE)
    d[m] = Inf
    a = which(sapply(r, function(x) any(x == m[1,1])))
    b = which(sapply(r, function(x) any(x == m[1,2])))
    if(a!=b) {
      r[[a]] = c(r[[a]], r[[b]])
      r[[b]] = NULL
    }
    if(length(r) == 1) {
      return(prod(x[m[1,],1]))
    }
  }
}

part2(inputTest) == 25272
part2(input)

