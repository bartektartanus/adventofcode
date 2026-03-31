require(stringi)

input <- stri_join(readLines("~/workspace-private/adventofcode/adventofcode2024/input03.txt"), collapse="")
input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
# part1
part1 = function(input) {
  x = stri_match_all_regex(input, "mul\\((\\d+),(\\d+)\\)")[[1]]
  sum(as.integer(x[,2]) * as.integer(x[,3]))
} 
part1(input)

# part2
m = stri_join(stri_extract_all_regex(input, "(^|do\\(\\)).*?(don't\\(\\)|$)")[[1]], collapse="")
part1(m)
