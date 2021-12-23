require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input09.txt")
input = stri_split_lines("2199943210
3987894921
9856789892
8767896789
9899965678")[[1]]
d = matrix(as.integer(unlist(stri_match_all_regex(input, "."))), nrow = length(input), byrow = TRUE)
d
low = 0
risk = 0
for(i in 1:100) {
  for(j in 1:100) {
    x = c(d[i-1,j] , d[i,j-1])
    if(i < 100) {
      x = c(x, d[i+1, j])
    }
    if(j < 100) {
      x = c(x, d[i,j+1])
    }
    if(all(x > d[i,j])) {
      #cat(i, " ", j)
      low = low +1 
      risk = risk + d[i,j] + 1
    }
  }
}
risk

size = function(d, i,j, currentBasin = matrix(FALSE, nrow(d), ncol(d))) {
  if(i < 1 | i > nrow(d) | j < 1 | j > ncol(d)) {
    return(currentBasin)
  } else if(currentBasin[i,j] | d[i,j] == 9){
    return(currentBasin)
  } else {
    currentBasin[i,j] = TRUE
    currentBasin = size(d, i+1, j, currentBasin)
    currentBasin = size(d, i-1, j, currentBasin)
    currentBasin = size(d, i, j+1, currentBasin)
    currentBasin = size(d, i, j-1, currentBasin)
    return(currentBasin)
  }
}
sum(size(d, 1,10))

sizes <- c()
for(i in 1:nrow(d)) {
  for(j in 1:ncol(d)) {
    x = c(d[i-1,j] , d[i,j-1])
    if(i < nrow(d)) {
      x = c(x, d[i+1, j])
    }
    if(j < ncol(d)) {
      x = c(x, d[i,j+1])
    }
    if(all(x > d[i,j])) {
      #cat(i, " ", j)
      sizes = c(sizes, sum(size(d, i,j)))
    }
  }
}
prod(sort(sizes, decreasing = TRUE)[1:3])
