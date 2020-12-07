input <- readLines("~/workspace-private/adventofcode2015/input05.txt")

require(stringi)

sum(stri_count_regex(input, '[aeiou]') >= 3 & stri_detect_regex(input, '(.)\\1') &
  !stri_detect_regex(input, 'ab|cd|pq|xy'))

sum(stri_detect_regex(input, '(?<double>..).*?\\k<double>') &
  stri_detect_regex(input, '(?<letter>.).\\k<letter>'))

