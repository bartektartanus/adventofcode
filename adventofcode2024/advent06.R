require("stringi")
input <- readLines("input06.txt")
inputTest = stri_split_lines("....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")[[1]]

part1 = function(input) {
  n = stri_length(input[1])
  m = t(simplify2array(stri_sub_all(input, cbind(1:n,1:n), use_matrix = TRUE)))
  d = list(-1:0, 0:1, 1:0,0:-1)
  di = 1
  p = which(m == "^")
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

