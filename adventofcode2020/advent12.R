input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input12.txt')
input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input12-test.txt')

require(stringi)

p <- c(0,0)

dm <- list(E=c(0,1), S=c(-1,0), W=c(0,-1), N=c(1,0))
d <- 1

x <- stri_match_first_regex(input, "([A-Z])(\\d+)")[,-1]
for(i in seq_len(nrow(x))){
  a <- x[i,1]
  b <- as.integer(x[i,2])
  if(a == "F"){
    p <- p + dm[[d]] * b  
  } else if (any(a == c("L","R"))){
    if(a == "L"){
      b <- 360 - b
    }
    b <- b/90
    d <- ((d-1 + b) %% 4) + 1
  } else {
    p <- p + dm[[a]] * b
  }
}
p
sum(abs(p))

## part 2


p <- c(0,0)
w <- c(1,10) # c(-10,1)
dm <- list(E=c(0,1), S=c(-1,0), W=c(0,-1), N=c(1,0))
d <- 1

x <- stri_match_first_regex(input, "([A-Z])(\\d+)")[,-1]
for(i in seq_len(nrow(x))){
  a <- x[i,1]
  b <- as.integer(x[i,2])
  if(a == "F"){
    p <- p + w * b  
  } else if (any(a == c("L","R"))){
    if(a == "L"){
      b <- 360 - b
    }
    b <- b/90
    for(i in 1:b){
      w <- c(-w[2], w[1])
    }
  } else {
    w <- w + dm[[a]] * b
  }
}
p
sum(abs(p))
