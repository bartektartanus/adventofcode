require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2015/input14.txt")
input <- c("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
           "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

x = stri_match_first_regex(input, "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+)")[,-(1:2)]
m = cbind(matrix(as.integer(x), ncol=3), 0,0)
m[,4] = m[,2] + m[,3]
m[,5] = m[,1] * m[,2]
m

t = 1000
t = 2503

distance <- function(m, t) {
  (t %/% m[,4]) * m[,5] + pmin(t %% m[,4], m[,2]) * m[,1]
}

max(distance(m, t))

points <- integer(nrow(m))
for(i in 1:2503) {
  d <- distance(m, i)
  w <- which.max(d)
  points[w] = points[w] + 1
}
max(points)
