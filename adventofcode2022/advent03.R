require(stringi)

input = readLines("~/workspace-private/adventofcode/adventofcode2022/input03.txt")

sum = 0
for(line in input) {
  l = stri_length(line)/2
  x = stri_sub(line, c(1, l+1), c(l, 2*l))
  a = stri_match_first_regex(x[1], stri_paste("[", x[2], "]", collapse = ""))[,1]
  sum = sum + which(c(letters, LETTERS) == a)
}
sum

# part 2

sum = 0
for(i in seq_len(length(input)/3)) {
  x = stri_match_all_regex(input[3*i-2], stri_paste("[", input[3*i-1], "]", collapse = ""))[[1]]
  y = stri_match_all_regex(input[3*i], stri_paste("[", stri_paste(x, collapse=""), "]", collapse = ""))[[1]]
  sum = sum + which(c(letters, LETTERS) == y[1,1])
}
sum
