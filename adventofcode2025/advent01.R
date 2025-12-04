require("stringi")
input <- readLines("input01.txt")
inputTest = stri_split_lines("L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")[[1]]

inputTest2 = "R1000"

part1 = function(input) {
  d <- 2 * (stri_sub(input, 1,1) == "R") - 1
  v <- as.integer(stri_sub(input, 2))
  
  sum(cumsum(c(50,d*v)) %% 100 == 0)
}

part1(inputTest)
part1(inputTest2)
part1(input)

part2 = function(input) {
  d <- 2 * (stri_sub(input, 1,1) == "R") - 1
  v <- as.integer(stri_sub(input, 2))
  i = sum(v %/% 100)
  v = v %% 100
  r = 50
  for(x in d*v) {
    nr = r + x
    if((r > 0 && nr <= 0) || nr > 99) {
      i = i + 1
    }
    r = nr %% 100
  }
  i
}

part2(inputTest)
part2(inputTest2)
part2(input)

