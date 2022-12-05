require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input16.txt")

rep = sort(apply(expand.grid(0:1, 0:1, 0:1, 0:1),1, function(x) paste0(x, collapse="")))
names(rep) = c(0:9,LETTERS[1:6])

binary = character(stri_length(input))
for(i in seq_len(stri_length(input))){
  s = stri_sub(input, i,i)
  binary[i] = rep[s]
}
binary = paste0(binary, collapse = "")
