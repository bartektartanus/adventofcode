input = readLines("~/workspace-private/adventofcode/adventofcode2023/input02.txt")
testInput = stri_split_fixed("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green","\n")[[1]]
require(stringi)

part1 = function(input) {
  sum(which(!stri_detect_regex(input, "(1[3-9]|[2-9]\\d) red|(1[4-9]|[2-9]\\d) green|(1[5-9]|[2-9]\\d) blue"))) 
}

part1(testInput)
part1(input)

part2 = function(input) {
  r = extractColor(input, "red")
  g = extractColor(input, "green")
  b = extractColor(input, "blue")
  sum(r*g*b)
}

extractColor = function(input, color) {
  x = stri_match_all_regex(input, stri_paste("(\\d+) ", color, collapse=""))
  sapply(x, function(x) max(as.integer(x[,2])))
}

part2(testInput)
part2(input)
