
input <- readLines("~/workspace-private/adventofcode2020/input03.txt")

require(stringi)
input <- t(simplify2array(stri_extract_all_regex(input, '.')))



checkSlope <- function(input, right = 3, down = 1){
  col <- 1
  count <- 0
  for(i in seq(1, nrow(input), by = down)){
    if(input[i, col] == '#'){
      count <- count +1    
    }
    col <- (col + right - 1) %% ncol(input) + 1
  }
  count
}

# part 1
checkSlope(input)

# part 2
checkSlope(input, 1) * checkSlope(input, 3) * checkSlope(input, 5) *
  checkSlope(input, 7) * checkSlope(input, 1, 2) 
