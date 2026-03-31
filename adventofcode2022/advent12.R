require(fastmap)

input = readLines("~/workspace-private/adventofcode/adventofcode2022/input12.txt")
inputTest = strsplit(c("Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"), "\n")[[1]]

propagate = function(pos, map, dist) {
  r = nrow(map)
  q = fastmap()
  q$set(as.character(pos), pos)
  
  while(q$size() > 0) {
    for(k in q$keys()) {
      pos = q$get(k)
      q$remove(k)
      for(np in (pos + c(-1,1,-r,r))) {
        if(np > 0 && np <= length(map) && abs(diff(c(np-1,pos-1)%%r)) <= 1 && abs(diff(c(np-1, pos-1) %/% r)) <= 1) {
          a = map[pos]
          b = map[np]
          if(b-a <= 1) {
            if(is.na(dist[np]) || dist[np] > dist[pos] + 1) {
              dist[np] <- dist[pos] + 1
              q$set(as.character(np), np)
            }
          }
          
        }
      }
    }
  }
  dist
}

part1 = function(input) {
  map = t(simplify2array(strsplit(input, "")))
  start = which(map=="S")
  end = which(map=="E")
  pos = start
  map[start] = "a"
  map[end] = "z"
  nr = nrow(map)
  map = matrix(match(map, letters), nrow=nr)
  dist <- map
  dist[,] <- NA
  dist[pos] <- 0
  dist = propagate(pos, map, dist)
  dist[end]
}

part1(inputTest)
part1(input)

# not the best solution, algorithm from part1 should be "reversed" to start from end and go down max 1 step
part2 = function(input) {
  map = t(simplify2array(strsplit(input, "")))
  start = which(map=="S")
  end = which(map=="E")
  map[start] = "a"
  map[end] = "z"
  nr = nrow(map)
  map = matrix(match(map, letters), nrow=nr)
  allDist = c()
  for(i in 1:length(map)) {
    if(map[i] == 1) {
      pos = i
      dist <- map
      dist[,] <- NA
      dist[pos] <- 0
      allDist = c(allDist, propagate(pos, map, dist)[end])
    }
    cat(length(allDist),"/",i," ", sep="")
  }
  min(allDist, na.rm=T)
}

part2(inputTest)
part2(input)
