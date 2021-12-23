require(stringi)
require(tidyverse)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input10.txt")

simplify = function(x) {
  stri_replace_all_fixed(x, c('()', '[]', '{}', '<>'), c(""), vectorize_all = FALSE)
}
simplifyAll = function(x) {
  s = x
  for(i in seq_len(stri_length(x))){
    s = simplify(s)
  }
  s
}
simplifyAll("[[[{]]]")
corrupted = input %>% map(simplifyAll) %>% stri_extract_first_regex("[\\)\\}\\>\\]]")
sum(table(corrupted) * c(3, 57, 1197, 25137))

incomplete = input[is.na(corrupted)]
r = stri_reverse(simplify2array(incomplete %>% map(simplifyAll)))

score = function(x) {
  s = 0
  p <- c('(' = 1, '[' = 2, '{' = 3, '<' = 4)
  for(i in seq_len(stri_length(x))) {
    s = 5 * s + p[stri_sub(x, i,i)]
  }
  s
}
score(r[2])
median(sapply(r, score))
