input <- readLines("~/workspace-private/adventofcode/adventofcode2023/input01.txt")
testInput = stri_split_fixed(c("two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"), "\n")[[1]]
require(stringi)

sum(as.integer(stri_join(stri_extract_first_regex(input, "\\d"), stri_extract_last_regex(input, "\\d"))))

# part 2

part2 = function(input) {
  numbers = stri_split_fixed("one, two, three, four, five, six, seven, eight, nine", ", ")[[1]]
  
  regex = stri_join(stri_join(numbers, collapse="|"), "\\d", sep="|")
  regexRev = stri_join(stri_reverse(stri_join(numbers, collapse="|")), "\\d", sep="|")
  indexedNumbers = 1:9
  names(indexedNumbers) = numbers
  
  first = stri_extract_first_regex(input, regex)
  last = stri_reverse(stri_extract_first_regex(stri_reverse(input), regexRev))
  
  f = as.integer(first)
  f[is.na(f)] = indexedNumbers[first[is.na(f)]]
  
  l = as.integer(last)
  l[is.na(l)] = indexedNumbers[last[is.na(l)]]
  
  sum(f*10 + l)
}

part2(input)
part2(testInput)
