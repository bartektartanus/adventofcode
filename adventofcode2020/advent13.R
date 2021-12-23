input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input13.txt')
input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input13-test.txt')

require(stringi)
require(bit64)
input

d <- as.integer(input[1])
t <- as.integer64.character(stri_split_regex(input[2], ',x?', omit_empty = TRUE)[[1]])

x <- d - d %% t + t
t[which.min(x)] * (min(x) - d)

input[2] <- '17,x,13,19'
t <- as.integer64.character(stri_split_regex(input[2], ',', omit_empty = TRUE)[[1]])
t
x <- which(!is.na(t))
x
a <- t[x]
m <- which.max(a)
d <- as.integer64(a[1] - x[1])
f <- 1
i <- a[f]
while(TRUE){
  b <- 0:f+1
  if(all(((d + x[b]) %% a[b])==0)){
    f <- f + 1
    if(f == length(x)){
      break
    }
    i <- prod(a[1:f])
    cat('\n',f, ' ', i)
    print(i)
  }
  d <- d + i
}

(d + x) %% a
d + 1

a
