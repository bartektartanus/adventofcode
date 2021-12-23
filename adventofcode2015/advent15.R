require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2015/input15.txt")
input <- c("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
           "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
input 

m <- stri_match_first_regex(input, " \\w+ (-?\\d+), \\w+ (-?\\d+), \\w+ (-?\\d+), \\w+ (-?\\d+), \\w+ (-?\\d+)")[,-1]
m = matrix(as.integer(m), ncol=5)
cal = m[,5]
m = m[,-5]

score1 <- 0
for(i in 1:97) {
  for(j in 1:(100-i)) {
    for(k in 1:(100-i-j)) {
      l = 100 - i - j - k
      v = c(i,j,k,l)
      s = prod(pmax(colSums(m * v), 0))
      score1 = max(s, score1) 
    }
  }
  cat(i)
}
score1

score2 <- 0
for(i in 1:97) {
  for(j in 1:(100-i)) {
    for(k in 1:(100-i-j)) {
      l = 100 - i - j - k
      v = c(i,j,k,l)
      s = prod(pmax(colSums(m * v), 0))
      if(sum(cal * v) == 500) {
        score2 = max(s, score2) 
      }
    }
  }
  cat(i)
}
score2
