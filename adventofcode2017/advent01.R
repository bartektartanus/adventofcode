input = readLines("~/workspace-private/adventofcode/adventofcode2017/input01.txt")

require(stringi)

input_cycle = stri_paste(input, stri_sub(input,1,1), collapse="")

sum(as.integer(stri_match_all_regex(input_cycle, "(\\d)(?=\\1)", opts_regex = stri_opts_regex())[[1]][,2]))

x = stri_sub(input, 1:stri_length(input), length=1)
n = length(x)
half = n/2
sum(as.integer(x[x == x[c((half+1):n, 1:half)]]))
