input = readLines("~/workspace-private/adventofcode/adventofcode2022/input25.txt")
inputTest = c("1121-1110-1=0")

toSnafu = function(x) {
  n = c()
  while(x!=0){
    r = (x+2) %% 5-2
    n = c(r+3, n)
    x = (x - r)/5
  }
  paste0(c("=","-",0,1,2)[n], collapse="")
}

for(i in 1:26) {
  cat("\n",i, " ", toSnafu(i))
}

part1 = function(input) {
  toSnafu(sum(sapply(strsplit(input, ""), function(x) sum(c("=" = -2, "-" = -1, "0" = 0, "1" = 1, "2" = 2)[x] * 5^((length(x)-1):0)))))
}

part1(inputTest)
part1(input)
