input <- as.integer64.character(readLines('~/workspace-private/adventofcode/adventofcode2020/input09.txt'))
require(gmp)
require(stringi)
library(bit64)

findFirstWrong <- function(x, control = 25){
  for(i in (control+1):length(x)){
    if(!isSum(x[i], x[i - 1:25])){
      print(i)
      print(x[i])
      break
    }
  }
}

isSum <- function(x, v){
  any(c(x - v) %in% v)
}

findFirstWrong(input)
x <- 375054920
s <- cumsum(input)
for(i in seq_along(input)){
  if(any(s %in% x)){
    print(i)
    print(s)
    print(which(s==x))
  }
  s <- s-input[i]
}
s

sum(input[c(488,504)])
sum(input[c(488:504)])
a <- input[c(488:504)]
min(a) + max(a)
