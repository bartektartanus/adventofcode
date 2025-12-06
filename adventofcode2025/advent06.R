require("stringi")
input <- readLines("input06.txt")
inputTest = stri_split_lines("123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")[[1]]

part1 = function(input) {
  x = stri_split_regex(input, " +", omit_empty = TRUE, simplify = TRUE)
  n = ncol(x)
  m = nrow(x)
  s = 0
  for(i in seq_len(n)) {
    if(x[m,i] == "+") {
      s = s + sum(as.numeric(x[-m,i]))
    } else {
      s = s + prod(as.numeric(x[-m,i]))
    }
  }
  s
}

part1(inputTest) == 4277556
format(part1(input), digits=22)

part2 = function(input) {
  s = 0
  op = sum
  e = c()
  n = stri_length(input[1])
  m = length(input)
  for(i in seq_len(n)) {
    o = stri_sub(input[m], i, i)
    if(o == "+") {
      op = sum
    } else if(o == "*") {
      op = prod
    }
    v = stri_paste(stri_sub(input[-m], i,i), collapse="")
    if(stri_isempty(stri_trim(v))) {
      s = s + op(e)
      e = c()
    } else {
      e = c(e, as.numeric(v))
    }
  }
  s = s + op(e)
  s
}

part2(inputTest) == 3263827
format(part2(input), digits=22)

