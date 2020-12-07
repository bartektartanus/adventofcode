input <- readLines('~/workspace-private/adventofcode2015/input08.txt')
input <- c('""', '"abc"', '"aaa\\"aaa"', '"\\x27"')
input[21]
require(stringi)
inMemory <- input
inMemory <- stri_replace_all_regex(inMemory, '^"',"")
inMemory <- stri_replace_all_regex(inMemory, '"$',"")
inMemory <- stri_replace_all_regex(inMemory, '\\\\x[0-9a-f][a-f0-9]',"A")
inMemory <- stri_replace_all_regex(inMemory, '\\\\\\\"',"Q")
inMemory <- stri_replace_all_regex(inMemory, '\\\\',"S")
inMemory[1:6]
sum(stri_length(input)) - sum(stri_length(inMemory))

