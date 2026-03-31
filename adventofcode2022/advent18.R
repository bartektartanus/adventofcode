require(stringi)
require(dplyr)
input = readLines("~/workspace-private/adventofcode/adventofcode2022/input18.txt")
inputExample = c("2,1,1", "1,1,1")
inputTest = c("2,2,2","1,2,2","3,2,2","2,1,2","2,3,2",
"2,2,1","2,2,3","2,2,4","2,2,6","1,2,5","3,2,5","2,1,5","2,3,5")
part1 = function(input) {
  sides = 0
  toVec = function(x) as.integer(stri_split_fixed(x, ",")[[1]])
  for(i in seq_along(input)) {
    sides = sides + 6
    for(j in seq_len(i-1)) {
      x = toVec(input[i])
      y = toVec(input[j])
      if(all(sort(abs(x - y)) == c(0,0,1))) {
        sides = sides - 2
      }
    }
  }
  sides
}
part1(inputExample)
part1(inputTest)
part1(input)

part2 = function(input) {
  r = function(input, dim) range(as.integer(sapply(stri_split_fixed(input, ","), function(x) x[[dim]]))) + c(-1,1)
  xRange = r(input, 1)
  yRange = r(input, 2)
  zRange = r(input, 3)
  
  queue = list(c(xRange[1], yRange[1], zRange[1]))
  seen = c()
  sides = 0
  inRange = function(p) between(p[1], xRange[1], xRange[2]) && 
    between(p[2], yRange[1], yRange[2]) &&
    between(p[3], zRange[1], zRange[2])
  neighbours = function(p) list(p + c(1,0,0), p + c(0,1,0), p + c(0,0,1), p - c(1,0,0), p - c(0,1,0), p - c(0,0,1))
  while(length(queue) > 0) {
    current = queue[[1]]
    queue = queue[-1]
    if(!any(paste0(current, collapse=",") == seen)) {
      seen = c(seen, paste0(current, collapse=","))
      for(n in neighbours(current)) {
        if(inRange(n)) {
          p = paste0(n, collapse=",")
          if(any(p == input)) {
            sides = sides+1
          } else {
            queue[length(queue)+1] = list(n) 
          }
        }
        
      }
    }
  }
  sides
  
}

part2(inputExample)
part2(inputTest)
part2(input)
