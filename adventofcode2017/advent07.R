require("stringi")
input <- readLines("input07.txt")
inputTest = stri_split_lines("pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")[[1]]

part1 = function(input) {
  m = stri_match_first_regex(input, "([a-z]+) \\(\\d+\\)( -> (.*))?$")[,-1]
  nodes = unlist(stri_split_fixed(m[,3], ", ", omit_empty = TRUE))
  nodes = nodes[!is.na(nodes)]
  h = m[,1]
  setdiff(h,nodes)
}

part1(inputTest) == "tknk"
part1(input)

weight = function(n, d) {
  x = d[n,]
  if(is.na(x$totalWeight)) {
    s = c()
    for(a in stri_split_fixed(x$down, ", ")[[1]]) {
      d = weight(a, d)
      s = c(s, d[a,]$totalWeight)
    }
    #cat("\nDEBUG n=",n," s=",s, " d=", d[n,3])
    if(length(unique(s)) == 1) {
      d[n,3] <- sum(s) + d[n,]$weight
    } else {
      cat("\nERROR n=",n," s=",s, " down=", x$down)
    }
  }
  d
}

part2 = function(input) {
  m = stri_match_first_regex(input, "([a-z]+) \\((\\d+)\\)( -> (.*))?$")[,c(-1,-4)]
  d <- data.frame(node = m[,1], weight = as.integer(m[,2]), totalWeight = NA, down = m[,3], row.names=m[,1])  
  w = is.na(d$down)
  d[w,3] = d[w,]$weight
  for(i in seq_len(nrow(d))) {
    d <- weight(d[i,1], d)
  }
  d
}

part2(inputTest)
d = part2(input)
d["ltleg",]
