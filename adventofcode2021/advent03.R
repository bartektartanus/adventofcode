require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input03.txt")
input <- stri_split_lines("00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")[[1]]



x <- c()
for(i in 1:stri_length(input[1])) {
  t <- table(stri_sub(input, i, len=1))
  x <- c(x, t[2]>t[1])
}

g <- stri_paste(as.integer(x), collapse="")
e <- stri_paste(as.integer(!x), collapse="")

gi <- sum(2^((stri_length(input[1]):1)-1) * x)
ei <- sum(2^((stri_length(input[1]):1)-1) * !x)
gi * ei


o2 <- input
for(i in 1:stri_length(o2[1])) {
  a <- stri_sub(o2, i, len=1)
  t <- table(a)
  if(t[2] >= t[1]){
    o2 <- o2[a == "1"]
  } else {
    o2 <- o2[a == "0"]
  }
  if(length(o2) == 1) {
    cat("zostalo 1 w iteracji ",i)
    break
  }
}


co2 <- input
for(i in 1:stri_length(co2[1])) {
  a <- stri_sub(co2, i, len=1)
  t <- table(a)
  if(t[2] >= t[1]){
    co2 <- co2[a == "0"]
  } else {
    co2 <- co2[a == "1"]
  }
  if(length(co2) == 1) {
    cat("zostalo 1 w iteracji ",i)
    break
  }
}

toLogical <- function(x){
  stri_sub(x, 1:stri_length(x)[1], len=1) == "1"
}

o2i <- sum(2^((stri_length(input[1]):1)-1) * toLogical(o2))
co2i <- sum(2^((stri_length(input[1]):1)-1) * toLogical(co2))
o2i * co2i
