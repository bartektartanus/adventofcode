input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input11.txt')
input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input11-test.txt')
input <- c(".............",".L.L.#.#.#.#.",".............")
require(stringi)

n <- stri_length(input[1])
x <- simplify2array(stri_sub_all(input, 1:n, len=1))
x
neighbours <- function(x, i, j){
  a <- max(1, i-1):min(nrow(x), i+1)
  b <- max(1, j-1):min(ncol(x), j+1)
  sum(x[a,b] == '#') - sum(x[i,j] == '#')
}

neighbours(x, 4, 2)

nextStep <- function(x, neighbours, maxCount){
  newX <- x
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      count <- neighbours(x, i, j)
      if(x[i,j] == 'L' & count == 0){
        newX[i,j] = '#'
      }else if(x[i,j] == '#' & count >= maxCount){
        newX[i,j] = 'L'
      }
    }
  }
  newX
}


iter <- function(x, nextStep, neighbours, maxCount = 4){
  prev <- x
  step <- nextStep(prev, neighbours, maxCount) 
  i <- 0
  while(any(step != prev)){
    i <- i + 1
    prev <- step
    step <- nextStep(prev, neighbours, maxCount) 
    cat("\niter",i)
  }
  sum(step == '#')
}

iter(x, nextStep, neighbours)


directions = matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), ncol = 2, byrow = TRUE)

hasNeighbor <- function(x, i, j, id, jd) {
  ii <- i + id
  jj <- j + jd
  #cat(ii, ' ', jd, '\n')
  while (ii >= 1 & ii <= nrow(x) & jj >= 1 & jj <= ncol(x)) {
    nextSeat = x[ii, jj]
    if (nextSeat == '#') {
      return(1)
    } else if (nextSeat == 'L') {
      return(0)
    }
    ii <- ii + id
    jj <- jj + jd
  }
  return(0)
}

neighbours2 <- function(x, i, j){
  count <- 0
  for(r in 1:nrow(directions)){
    count <- count + hasNeighbor(x, i, j, directions[r,1],directions[r,2])
  }
  count
}
neighbours2(x, 2,2)
iter(x, nextStep, neighbours2, 5)
