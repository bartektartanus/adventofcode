
require(stringi)
input <- "1113122113"

lookAndSay <- function(input) {
  x <- rle(stri_sub(input, 1:stri_length(input), len=1))
  stri_paste(x$lengths, x$values, collapse = "")
}

current <- input
for(i in seq_len(50)){
  current <- lookAndSay(current)
}
stri_length(current)
