require("stringi")
input <- 361527

part1 = function(input) {
  p = c(0,0)
  d = list(c(1,0), c(0,1), c(-1,0), c(0,-1))
  i = 1
  s = 1
  while(s < input) {
    l = i %/% 2
    p = p + l * d[[(i-1)%%4+1]]
    i = i + 1
    s = s + l
    cat("\np=",p," l=", l, " i=", i, " s=", s)
  }
  sum(abs(p - (s-input) * d[[(i-2)%%4+1]]))
}

part1(12) == 3
part1(23) == 2
part1(input)

part2 = function(input) {
  m = matrix(0, 10, 10)
  p = c(5,5)
  m[p[1], p[2]] = 1
  d = list(c(1,0), c(0,1), c(-1,0), c(0,-1))
  i = 1
  s = 1
  while(TRUE) {
    l = i %/% 2
    for(j in seq_len(l)) {
      p = p + d[[(i-1)%%4+1]]
      v = sum(m[p[1]+(-1):1, p[2] + (-1):1])
      m[p[1], p[2]] = v
      if(v > input) {
        return(v)
      }
    }
    i = i + 1
  }
}

part2(inputTest)
part2(input)

