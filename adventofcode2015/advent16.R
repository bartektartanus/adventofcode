require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2015/input16.txt")

sueAll = stri_replace_first_regex(input, "Sue \\d+: ", "")

tape = c(children=3,
      cats=7,
      samoyeds=2,
      pomeranians=3,
      akitas=0,
      vizslas=0,
      goldfish=5,
      trees=3,
      cars=2,
      perfumes=1)

# part 1
for(i in seq_along(sueAll)) {
  sue = sueAll[i]
  x = stri_match_all_regex(sue, "(\\w+): (\\d+)")[[1]][,-1]
  if(all(tape[x[,1]] == as.integer(x[,2]))) {
    cat("sue=", sue, " i=",i)
  }
}

# part2
greater = function(x, name) {
  all(as.integer(x[x[,1] == name,2]) > tape[name])
}

fewer = function(x, name) {
  all(as.integer(x[x[,1] == name,2]) < tape[name])
}
for(i in seq_along(sueAll)) {
  sue = sueAll[i]
  x = stri_match_all_regex(sue, "(\\w+): (\\d+)")[[1]][,-1]
  x
  y = matrix(x[!(x[,1] %in% c("trees","cats","pomeranians","goldfish")),], ncol=2)
  if(all(tape[y[,1]] == as.integer(y[,2])) && greater(x, "trees") && greater(x, "cats") && fewer(x,"pomeranians") && fewer(x,"goldfish")) {
    cat("sue=", sue, " i=",i)
  }
}
