require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2022/input02")
result = c("R P" = 1, "P S" = 1, "S R" = 1, 
           "R R" = 0, "P P" = 0, "S S" = 0,
           "R S" = -1, "S P" = -1, "P R" = -1)
score = 0
aMap = c(A="R", B="P", C="S")
bMap = c(X="R", Y="P", Z="S")
points = c(R=1, P=2, S=3)
for(line in input) {
  x = stri_split_fixed(line, " ")[[1]]
  a = aMap[x[1]]
  b = bMap[x[2]]
  p = points[b]
  score = score + p
  score = score + 3*(result[stri_join(a, " ", b, collapse="")] + 1)
}

# part 2
score = 0
for(line in input) {
  x = stri_split_fixed(line, " ")[[1]]
  a = aMap[x[1]]
  if(x[2] == "Y") {
    score = score + 3 + points[a]
  } else if(x[2] == "X") { 
    # lose
    for(i in c("R", "P", "S")) {
      if(result[stri_join(a, " ", i, collapse="")] == -1) {
        score = score + points[i]
      }
    }
  } else {
    #win
    for(i in c("R", "P", "S")) {
      if(result[stri_join(a, " ", i, collapse="")] == 1) {
        score = score + points[i] + 6
      }
    }
  }
}
