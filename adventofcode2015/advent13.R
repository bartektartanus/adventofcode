require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2015/input13.txt")

i <- stri_replace_all_fixed(input, c("gain ", "lose "), c("","-"), vectorize_all = FALSE)
x <- stri_match_first_regex(i, "^(\\w+) would (-?\\d+) .*? (\\w+).$")[,c(2,4,3)]

t <- cbind(x, x[order(x[,2]),c(2,1,3)])
t <- data.frame(a=t[,1], b=t[,2], c=as.integer(t[,3]) + as.integer(t[,6]))
t
