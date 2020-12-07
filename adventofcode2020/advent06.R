input <- readLines("~/workspace-private/adventofcode2020/input06.txt")

require(stringi)

a <- stri_match_all_regex(stri_join(input, collapse = '\n'), '([a-z]+\n)+(\n|$)')[[1]][,1]

countAnyLetters <- function(x){
  length(unique(stri_extract_all_regex(x, '[a-z]')[[1]]))
}

countAllLetters <- function(x){
  b <- stri_extract_all_regex(x, '[a-z]+(\n|$)')[[1]]
  d <- stri_extract_all_regex(b, '[a-z]')
  length(Reduce(intersect, d, d[[1]]))
}

# part1
sum(sapply(a, countAnyLetters))
# part2
sum(sapply(a, countAllLetters))
