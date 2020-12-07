require(stringi)

input <- as.matrix(read.fwf("~/adventofcode2019/input24.txt", rep(1,5), sep="1", comment.char=""))
input <- as.matrix(read.fwf("~/adventofcode2019/input24-test.txt", rep(1,5), sep="1", comment.char=""))
input <- cbind(".", input, ".")
input <- rbind(".", input, ".")
input
current <- input == "#"
count = 0
hashes <- rep(0, 10000)
hash <- calculateHash(current[2:6, 2:6])
while(!is.element(hash, hashes)){
  #print(current)
  hashes[count] <- hash
  current = gameOfLife(current)
  hash <- calculateHash(current[2:6, 2:6])
  count <- count + 1
  if(count %% 10000 == 0){
    cat(" ", count)
  }
}
hash
current

calculateHash <- function(m){
  sum(m * matrix(2^(0:24), 5,5,TRUE))
}

gameOfLife <- function(m){
  n <- m[-(1:2),c(-1,-7)] + m[-(6:7),c(-1,-7)] + m[c(-1,-7),-(1:2)] + m[c(-1,-7),-(6:7)]
  n
  a <- m[c(-1,-7), c(-1,-7)]
  b <- a
  b[a & n != 1] <- FALSE
  b[!a & (n == 1 | n == 2)] <- TRUE
  cbind(FALSE, rbind(FALSE, b, FALSE), FALSE)
}
prev
