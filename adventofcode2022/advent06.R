require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2022/input06.txt")

find = function(input, len) {
  for(i in seq_len(stri_length(input))) {
    x = stri_sub(input, 1:len - 1 + i, length = 1)
    if(length(x) == length(unique(x))) {
      return(i+len-1)
    }
  }
}

find(input, 4)
find(input, 14)
