require("stringi")
input <- readLines("input03.txt")
inputTest = stri_split_lines("987654321111111
811111111111119
234234234234278
818181911112111")[[1]]

part1 = function(input, size = 2) {
  s = 0
  for(x in input) {
    n = stri_length(x)
    a = as.integer(stri_sub(x, 1:n, len = 1))
    b = 0
    for(i in size:1) {
      b = b + which.max(a[(b+1):(n-i+1)])
      s = s + a[b] * 10^(i-1)
    }
  }
  s
}

part1(inputTest) == 357
part1(input)

part2 = function(input) {
  part1(input, 12)
}

part2(inputTest) == 3121910778619
format(part2(input), dig=22)

