require(stringi)
input <- sapply(readLines("~/workspace-private/adventofcode/adventofcode2021/input11.txt"), function(x) stri_sub(x, 1:10,1:10))
m = matrix(as.integer(input), 10, 10, byrow=TRUE)
m = rbind(NA, cbind(NA, m, NA), NA)
m
f = 0
for(k in 1:1000) {
  m = m + 1
  b = m == -1
  while(any(m>9 & !b, na.rm = T)){
    for(i in 2:11) {
      for(j in 2:11) {
        if(m[i,j] > 9 && !b[i,j]) {
          m[(i-1):(i+1), (j-1):(j+1)] = m[(i-1):(i+1), (j-1):(j+1)] + 1
          b[i,j] = TRUE
        }
      }
    }  
  }
  m[b] = 0
  if(sum(b, na.rm = T) == 100) {
    cat("all", k)
    break
  }
  f = f + sum(b, na.rm = T)
}
f

