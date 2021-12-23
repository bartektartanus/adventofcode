require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2015/input12.txt")

sum(as.integer(stri_extract_all_regex(input, "-?\\d+")[[1]]))

input2 <- stri_replace_all_regex(input, "\\{[^{]*\"red\"[^}]*?\\}","")
input2 <- stri_paste(readLines("~/workspace-private/adventofcode/adventofcode2015/input12b.json"), collapse = "")
sum(as.integer(stri_extract_all_regex(input2, "-?\\d+")[[1]]))
