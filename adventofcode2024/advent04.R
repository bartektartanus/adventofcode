require(stringi)

input <- readLines("~/workspace-private/adventofcode/adventofcode2024/input04.txt")

# part1
n = length(input)
byRow = input
byCol = input
diag1 = character(n * 2 - 1)
diag2 = character(n * 2 - 1)
for(i in 1:stri_length(input[1])) {
  byCol[i] = stri_paste(stri_sub(input, i, len=1), collapse="")
  diag1[i] = stri_paste(stri_sub(input[1:i], i:1, len=1), collapse="")
  diag1[n*2 - i] = stri_paste(stri_sub(input[(n-i+1):n], n:(n-i+1), len=1), collapse="")
  diag2[i] = stri_paste(stri_sub(input[1:i], (n-i+1):n, len=1), collapse="")
  diag2[n*2 - i] = stri_paste(stri_sub(input[(n-i+1):n], 1:i, len=1), collapse="")
}
sum(stri_count_fixed(byRow, "XMAS") + stri_count_fixed(byRow, "SAMX") +
  stri_count_fixed(byCol, "XMAS") + stri_count_fixed(byCol, "SAMX")) + 
  sum(stri_count_fixed(diag1, "XMAS") + stri_count_fixed(diag1, "SAMX") +
        stri_count_fixed(diag2, "XMAS") + stri_count_fixed(diag2, "SAMX"))

# part2
count = 0
for(i in 2:(n-1)) {
  for(j in 2:(n-1)) {
    if(stri_sub(input[i], j,j) == "A") {
      a = sort(stri_sub(input[i + c(-1,1)], j+c(-1,1),len=1))
      b = sort(stri_sub(input[i + c(-1,1)], j+c(1,-1),len=1))
      if(all(a == b) && all(a == c("M","S")))  {
        count = count +1
      }
    }
  }
}
count
