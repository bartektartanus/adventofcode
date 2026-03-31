input = readLines("~/workspace-private/adventofcode/adventofcode2023/input03.txt")
inputTest = stri_split_fixed("467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..","\n")[[1]]
require(stringi)

part1 = function(input) {
  input = c(stri_dup(".", stri_length(input[1])), input, stri_dup(".", stri_length(input[1])))
  input = stri_join(".", input, ".")
  rows = stri_locate_all_regex(input, "\\d+", omit_no_match=TRUE)
  
  s = 0
  for(i in seq_along(rows)){
    row = rows[[i]]
    for(j in seq_len(nrow(row))){
      up = stri_sub(input[i-1], row[j,1]-1, row[j,2]+1)
      this = stri_sub(input[i], c( row[j,1]-1, row[j,2]+1), length = 1)
      down = stri_sub(input[i+1], row[j,1]-1, row[j,2]+1)
      isPart = !stri_detect_regex(stri_join(up, this, down, collapse=""), "^\\.+$")
      if(isPart) {
        s = c(s,as.integer(stri_sub(input[i], row[j,1], row[j,2])))
      }
    }
  }
  sum(s)
}

part1(inputTest)
part1(input)

part2 = function(input) {
  input = c(stri_dup(".", stri_length(input[1])), input, stri_dup(".", stri_length(input[1])))
  input = stri_join(".", input, ".")
  gears = stri_locate_all_fixed(input, "*", omit_no_match = TRUE)
  rows = stri_locate_all_regex(input, "\\d+", omit_no_match=TRUE)
  
  s = 0
  for(i in seq_along(gears)){
    gear = gears[[i]]
    for(j in seq_len(nrow(gear))){
      g = gear[j,1]
      f = NULL
      x = 0:2 + g - 1
      for(r in (0:2 + i - 1)) {
        row = rows[[r]]
        for(k in seq_len(nrow(row))) {
          if(length(intersect(row[k,1]:row[k,2], x)) > 0){
            f = c(f, as.integer(stri_sub(input[r], row[k,1], row[k,2])))
          }
        }
      }
      if(length(f) == 2) {
        s = s + f[1]*f[2]
      }
    }
  }
  s
}

part2(inputTest)
part2(input)
