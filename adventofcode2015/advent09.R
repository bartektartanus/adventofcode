input <- readLines('~/workspace-private/adventofcode/adventofcode2015/input09.txt')
input <- c("London to Dublin = 464","London to Belfast = 518","Dublin to Belfast = 141")
require(stringi)

input

x <- stri_match_first_regex(input, '^(.*?) to (.*?) = (\\d+)$')[,-1]

planets <- unique(as.vector(x[,1:2]))

distance <- matrix(0, length(planets),length(planets), dimnames = list(planets, planets))

for(i in seq_len(nrow(x))){
  distance[x[i,1], x[i, 2]] <- as.integer(x[i,3])
  distance[x[i,2], x[i, 1]] <- as.integer(x[i,3])
}

paths <- list()

findMinPath <- function(p, rest){
  if(length(rest) == 1){
    return(distance[p, rest])
  }
  pathLength <- Inf
  for(i in seq_along(rest)){
    currentLength <- distance[p, rest[i]] + findMinPath(rest[i], rest[-i])
    pathLength <- min(pathLength, currentLength)    
  }
  pathLength
}
for(i in seq_along(planets)){
  print(findMinPath(planets[i], planets[-i]))
}


findMaxPath <- function(p, rest){
  if(length(rest) == 1){
    return(distance[p, rest])
  }
  pathLength <- 0
  for(i in seq_along(rest)){
    currentLength <- distance[p, rest[i]] + findMaxPath(rest[i], rest[-i])
    pathLength <- max(pathLength, currentLength)    
  }
  pathLength
}
for(i in seq_along(planets)){
  print(findMaxPath(planets[i], planets[-i]))
}

