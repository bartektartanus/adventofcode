require(stringi)

input <- readLines("~/workspace-private/adventofcode/adventofcode2024/input05.txt")


i = which(input == "")
rules = stri_split_fixed(input[1:(i-1)], "|", simplify = TRUE)
updates = input[-(1:i)]

# part 1
s = 0
for(u in updates) {
  u = as.integer(stri_split_fixed(u, ",")[[1]])
  m = u[(length(u)+1)/2]
  for(i in seq_len(nrow(rules))) {
    a = which(u == rules[i,1])
    b = which(u == rules[i,2])
    if(!all(diff(c(a,b)) > 0)) {
      m = 0
      break
    }
  }
  s = s + m
}
s #5948

s = 0
for(u in updates) {
  u = stri_split_fixed(u, ",")[[1]]
  x = sort(table(rules[rules[,1] %ix% u & rules[,2] %in% u,1]), decreasing = TRUE)
  if(all(names(x) == head(u, -1))) {
    m = u[(length(u)+1)/2]
    s = s + as.integer(m)
  }
}
s

# part 2
s = 0
for(u in updates) {
  u = stri_split_fixed(u, ",")[[1]]
  m = u[(length(u)+1)/2]
  x = sort(table(rules[rules[,1] %in% u & rules[,2] %in% u,1]), decreasing = TRUE)
  if(!all(names(x) == head(u, -1))) {
    m = names(x)[(length(u)+1)/2]
    s = s + as.integer(m)
  }
}
s
