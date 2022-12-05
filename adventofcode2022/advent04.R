require(stringi)

input = readLines("~/workspace-private/adventofcode/adventofcode2022/input04.txt")

sum = 0
for(line in input) {
  x = as.integer(stri_match_first_regex(line, "(\\d+)-(\\d+),(\\d+)-(\\d+)")[,-1])
  if((x[1] <= x[3] && x[2] >= x[4]) || (x[1] >= x[3] && x[2] <= x[4])) {
    sum = sum+1
  }
}
sum

# part 2
sum = 0
for(line in input) {
  x = as.integer(stri_match_first_regex(line, "(\\d+)-(\\d+),(\\d+)-(\\d+)")[,-1])
  a = x[1]:x[2]
  b = x[3]:x[4]
  if(length(unique(c(a,b))) != sum(length(c(a,b)))) {
    sum = sum + 1
  }
}
sum
