require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input06.txt")
testInput = stri_split_fixed("Time:      7  15   30
Distance:  9  40  200","\n")[[1]]

part1 = function(x) {
  races = lapply(stri_extract_all_regex(x, "\\d+"), as.integer)
  r = 1
  for(i in seq_along(races[[1]])) {
    t = races[[1]][i]
    d = races[[2]][i]
    c = 0
    for(j in 1:t) {
      if(j * (t-j) > d) {
        c = c+1
      }
    }
    r = r * c
  }
  r
}

part1(testInput)
part1(input)

part2 = function(x) {
  races = as.double(stri_extract_first_regex(stri_replace_all_fixed(x," ",""), "\\d+"))
  t = races[1]
  d = races[2]
  c = 0
  for(j in 1:t) {
    if(j * (t-j) > d) {
      c = c+1
    }
  }
  c
}

part2(testInput)
part2(input)
