require(stringi)
input <- readLines("~/adventofcode2019/input8.txt")
res <- c(25, 6)
layers <- stri_sub(input, seq(1, stri_length(input), 150), length = 150)
fewestZeros <- which.min(stri_count_fixed(layers, "0"))
prod(stri_count_fixed(layers[fewestZeros], 1:2))

layers

stri_replace_all_fixed()
??replace
i <- 1
result <- rep(0, 150)
for(i in 1:150){
  pixel <- stri_sub(layers,i,i)
  result[i] <- pixel[pixel != 2][1]
}
cat(stri_replace_all_fixed(stri_replace_all_fixed(
  stri_replace_all_regex(stri_paste(result, collapse = ""), "(\\d{25})","$1\n"),
  "0"," "), "1","#"))

