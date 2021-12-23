require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input06.txt")
input = "3,4,3,1,2"
x = as.integer(stri_split_fixed(input, ",")[[1]])

for(i in 1:80){
  x = x-1
  n = x== -1
  x[n] = 6
  x = c(x, rep(8, sum(n)))
  print(i)
}

length(x)

t <- table(x)
t
for(i in 1:256) {
  a = as.integer(names(t)) - 1
  b = a == -1
  if(any(b)){
    a[b] = 6
    names(t) = a
    t['8'] = sum(t[b])
  }else {
    names(t) = a
  }
}
format(sum(t), digits = 22)
