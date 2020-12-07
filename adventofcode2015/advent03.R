input <- readLines("~/workspace-private/adventofcode2015/input03.txt")

require(stringi)
# part 1
houses <- character(stri_length(input))

current <- c(0,0)
houses[1] <- stri_paste(current, collapse = ',')
go <- list('v' = c(0,-1), '^' = c(0,1), '<' = c(-1,0), '>' = c(1,0))
for(i in seq_along(houses)){
  direction <- stri_sub(input, i, i)
  current <- current + go[[direction]]
  houses[i+1] <- stri_paste(current, collapse = ',')
}

length(unique(houses))  

# part 2
houses <- character(stri_length(input))

santa <- c(0,0)
robo <- c(0,0)
houses[1] <- stri_paste(santa, collapse = ',')
go <- list('v' = c(0,-1), '^' = c(0,1), '<' = c(-1,0), '>' = c(1,0))
for(i in seq(1, length(houses), by = 2)){
  direction <- stri_sub(input, i, i)
  santa <- santa + go[[direction]]
  houses[i+1] <- stri_paste(santa, collapse = ',')
  direction <- stri_sub(input, i+1, i+1)
  robo <- robo + go[[direction]]
  houses[i+2] <- stri_paste(robo, collapse = ',')
}

length(unique(houses))  
