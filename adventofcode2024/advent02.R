input <- readLines("~/workspace-private/adventofcode/adventofcode2024/input02.txt")
input <- readLines("~/workspace-private/adventofcode/adventofcode2024/input02-test.txt")

# part1
sum(sapply(input, function(line) {
  x = as.integer(strsplit(line, " ")[[1]])
  d = diff(x)
  all(d == 1 | d == 2 | d == 3) | all(d == -1 | d == -2 | d == -3)
}))

# part2
sum(sapply(input, function(line) {
  x = as.integer(strsplit(line, " ")[[1]])
  d = diff(x)
  r = all(d == 1 | d == 2 | d == 3) | all(d == -1 | d == -2 | d == -3)
  if(r) {
    r
  } else {
    for(i in seq_along(x)) {
      y = x[-i]
      d = diff(y)
      r = all(d == 1 | d == 2 | d == 3) | all(d == -1 | d == -2 | d == -3)
      if(r) {
        return(r) 
      }
    }
    FALSE
  }
}))
