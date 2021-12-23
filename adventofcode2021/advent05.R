require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input05.txt")

x <- matrix(as.integer(stri_match_first_regex(input, "(\\d+),(\\d+) -> (\\d+),(\\d+)")[,-1]), ncol=4)

i1 <- x[x[,1] == x[,3] | x[,2] == x[,4],]
i2 <- x[x[,1] != x[,3] & x[,2] != x[,4],]
range(x)

m <- matrix(0, nrow=1000, ncol=1000)

for(i in seq_len(nrow(i1))) {
  r <- i1[i,]
  m[r[1]:r[3], r[2]:r[4]] <- m[r[1]:r[3], r[2]:r[4]] + 1
}

sum(m>1)

for(i in seq_len(nrow(i2))) {
  row <- i2[i,]
  r <- row[1]:row[3]
  c <- row[2]:row[4]
  if(length(r) != length(c)){
    print("error")
  }
  for(k in seq_along(r)){
    m[r[k], c[k]] <- m[r[k], c[k]] + 1
  }
}

sum(m>1)
