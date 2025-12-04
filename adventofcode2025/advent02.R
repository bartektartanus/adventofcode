require("stringi")
input <- readLines("input02.txt")
inputTest = stri_split_lines(
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
)[[1]]

part1 = function(input) {
  a = stri_split_fixed(input, ",")[[1]]
  x = stri_split_fixed(a, "-")
  s = 0
  for (e in x) {
    chars = as.character(e[1]:e[2])
    n = stri_length(chars)
    chars = chars[n %% 2 == 0]
    n = n[n %% 2 == 0]
    b = stri_sub(stri_dup(chars, 2), n %/% 2 + 1, len = n)
    s = s + sum(as.numeric(chars[b == chars]))
  }
  s
}

part1(inputTest) == 1227775554
part1(input)

part2 = function(input) {
  a = stri_split_fixed(input, ",")[[1]]
  x = stri_split_fixed(a, "-")
  s = 0
  for (e in x) {
    chars = as.character(e[1]:e[2])
    n = stri_length(chars)
    b = stri_sub(stri_dup(chars, 2), 2, 2 * n - 1)
    s = s + sum(as.numeric(chars[stri_detect_fixed(b, chars)]))
  }
  s
}

part2(inputTest) == 4174379265
part2(input)
