require("stringi")
input <- readLines("input04.txt")
inputTest = stri_split_lines("..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")[[1]]

inputTest2 = ""

part1 = function(input) {
  n = stri_length(input)[1]
  m = c(stri_dup(".", n+2), stri_pad_both(input, n+2, "."), stri_dup(".", n+2))
  s = 0
  r = (1:n) + 1
  for(i in r) {
    for(j in r) {
      if(stri_sub(m[i], j,j) == "@" && sum(stri_count_fixed(stri_sub(m[i + (-1:1)], j-1,j+1), "@")) < 5) {
        s = s+1
      }
    }
  }
  s
}

part1(inputTest) == 13
part1(input)

part2 = function(input) {
  n = stri_length(input)[1]
  m = c(stri_dup(".", n+2), stri_pad_both(input, n+2, "."), stri_dup(".", n+2))
  ps = -1
  r = (1:n) + 1
  while (ps != s) {
    ps = s
    for(i in r) {
      for(j in r) {
        if(stri_sub(m[i], j,j) == "@" && sum(stri_count_fixed(stri_sub(m[i + (-1:1)], j-1,j+1), "@")) < 5) {
          s = s+1
          stri_sub(m[i], j, j) = "."
        }
      }
    }
  }
  s
  
}

part2(inputTest) == 43
part2(input)

