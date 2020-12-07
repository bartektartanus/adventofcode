input <- readLines("~/workspace-private/adventofcode2015/input02.txt")

require(stringi)

x <- stri_match_first_regex(input, '(\\d+)x(\\d+)x(\\d+)')[,-1]

area <- function(x){
  x <- as.integer(x)
  areas <- c(x[1]*x[2], x[1]*x[3], x[2] * x[3])
  sum(areas *2 )+ min(areas)
}
# part 1
sum(apply(x, 1, area))

ribbon <- function(x){
  x <- as.integer(x)
  2*(sum(x) - max(x)) + prod(x)
}

# part 2
sum(apply(x, 1, ribbon))
