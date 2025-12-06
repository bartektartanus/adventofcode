require("stringi")
input <- as.integer(readLines("input05.txt"))
inputTest <- as.integer(stri_split_lines("0
3
0
1
-3")[[1]])
part1 = function(input) {
  i = 0
  s = 0
  while(i >= 0 && i < length(input)) {
    v = input[i + 1]
    input[i+1] = input[i+1] + 1
    i = i + v
    s = s + 1
  }
  s
}

part1(inputTest) == 5
part1(input)

part2 = function(input) {
  i = 0
  s = 0
  while(i >= 0 && i < length(input)) {
    v = input[i + 1]
    if(v >= 3) {
      input[i+1] = input[i+1] - 1
    } else {
      input[i+1] = input[i+1] + 1  
    }
    i = i + v
    s = s + 1
  }
  s
}


part2(inputTest) == 10
part2(input)

