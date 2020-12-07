input <- readLines("~/workspace-private/adventofcode2020/input05.txt")

require(stringi)
rows <- stri_sub(input, 1, 7)
cols <- stri_sub(input, 8)

binRows <- stri_replace_all_fixed(rows, c("F","B"), 0:1, FALSE )
binCols <- stri_replace_all_fixed(cols, c("L","R"), 0:1, FALSE )

binToInt <- function(x, n = stri_length(x)){
  sum(as.integer(stri_sub(x, 1:n, len = 1)) * 2 ^ (n:1-1))
}
seat <- integer(length(input))
for(i in seq_along(input)){
  row <- binToInt(stri_replace_all_fixed(stri_sub(input[i], 1, 7), c("F","B"), 0:1, FALSE ))
  col <- binToInt(stri_replace_all_fixed(stri_sub(input[i], 8), c("L","R"), 0:1, FALSE ))
  seat[i] <- row*8 + col  
}
# part 1
max(seat)

# part 2
setdiff(min(seat):max(seat), seat)
