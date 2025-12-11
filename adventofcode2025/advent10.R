require("stringi")
require("lpSolve")
input <- readLines("input10.txt")
inputTest = stri_split_lines("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")[[1]]

solveXor = function(current, expected, buttons, steps = 0) {
  if(all(current == expected)) {
    return(steps)
  }
  r = Inf
  for(i in seq_along(buttons)) {
    cur = xor(current, buttons[[i]])
    r = min(r, solveXor(cur, expected, buttons[-(1:i)], steps + 1))
  }
  r
}

part1 = function(input) {
  s = 0
  for(x in input) {
    e = stri_match_all_regex(stri_match_first_regex(x, "\\[(.+?)\\]")[,2], ".")[[1]] == "#"
    b = stri_split_fixed(stri_match_all_regex(x, "\\(([0-9,]+)\\)")[[1]][,2], ",")
    b = lapply(b, function(x)  1:length(e) %in% (as.integer(x) + 1))
    s = s + solveXor(rep(FALSE, length(e)), e, b)
  }
  s
}

part1(inputTest) == 7
part1(input)

part2 = function(input) {
  s = 0
  for(x in input) {
    e = as.integer(stri_match_all_regex(stri_match_first_regex(x, "\\{([\\d,]+)\\}")[,2], "\\d+")[[1]])
    b = stri_split_fixed(stri_match_all_regex(x, "\\(([0-9,]+)\\)")[[1]][,2], ",")
    b = sapply(b, function(x)  as.integer(1:length(e) %in% (as.integer(x) + 1)))
    objective.fn = rep(1, times=ncol(b))
    const.mat = b
    const.dir = rep("=", times=nrow(b))
    const.rhs = e
    lp.solution = lp("min", objective.fn, const.mat, 
                      const.dir, const.rhs, compute.sens=TRUE, int.vec=1:ncol(b))
    s = s + lp.solution$objva
  }
  s
}

part2(inputTest) == 33
part2(input)

