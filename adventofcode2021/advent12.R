require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input12.txt")

n = list()
for(i in input) {
  s = stri_split_fixed(i, "-")[[1]]
  n[[s[1]]] = c(n[[s[1]]], s[2])
  n[[s[2]]] = c(n[[s[2]]], s[1])
}
n

isSmallCave = function(x) {
  x != "start" & x!="end" & stri_detect_regex(x, "[a-z]+")
}
isSmallCave("start")
paths = function(current = "start") {
  last = tail(current, 1)  
  possible = setdiff(n[[last]], current[isSmallCave(current)])
  possible = setdiff(possible, "start")
  sum = 0
  for(p in possible) {
    if(p == "end") {
      cat("\nend = ", current, p)
      sum = sum + 1
    } else {
      sum = sum + paths(c(current, p)) 
    }
  }
  sum
}

paths()

paths2 = function(current = "start") {
  last = tail(current, 1)  
  if(all(table(current[isSmallCave(current)]) == 1)) {
    possible = n[[last]]
  } else {
    possible = setdiff(n[[last]], current[isSmallCave(current)])
  }
  possible = setdiff(possible, "start")

  sum = 0
  for(p in possible) {
    if(p == "end") {
      sum = sum + 1
    } else {
      sum = sum + paths2(c(current, p)) 
    }
  }
  sum
}

paths2()
