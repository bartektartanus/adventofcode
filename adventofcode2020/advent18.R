input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input18.txt')

input <- c('1 + (2 * 3) + (4 * (5 + 6))',
'2 * 3 + (4 * 5)',
'5 + (8 * 3 + 9 + 3 * 4 * 3)',
'5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))',
'((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2')

require(stringi)

calculate <- function(e){
  while(!stri_detect_regex(e, "^\\d+$")){
    x <- stri_match_first_regex(e, "(\\d+) (\\*|\\+) (\\d+)")
    if(x[,3] == '+'){
      r <- sum(as.numeric(x[,c(2,4)]))
    }else{
      r <- prod(as.numeric(x[,c(2,4)]))
    }
    e <- stri_replace_first_fixed(e, x[,1], r)
    e <- stri_replace_first_regex(e, '\\((\\d+)\\)', "$1")
  }
  return(as.numeric(e))
}

calculate2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 13632
calculate("7 * ((5 + 3 * 2 + 5) * (9 * 2 + 2 * 7 + 7 * 6) * (2 + 7 + 2 * 9)) * 8 * 7 * 7 + 2")

print(sum(sapply(input, calculate)), 22)


calculate2 <- function(e){
  while(!stri_detect_regex(e, "^\\d+$")){
    e <- calculateOnce(e)
  }
  return(as.numeric(e))
}

calculateOnce <- function(e){
  x <- stri_match_first_regex(e, "\\(((\\d+) (\\+) (\\d+))")
  if(any(is.na(x))){
    x <- stri_match_first_regex(e, "((\\d+) (\\+) (\\d+))")
  }
  if(any(is.na(x))){
    x <- stri_match_first_regex(e, "\\+ \\(((\\d+) (\\*|\\+) (\\d+))")
  }
  if(any(is.na(x))){
    x <- stri_match_first_regex(e, "((\\d+) (\\*) (\\d+))")
  }
  if(x[,4] == '+'){
    r <- sum(as.numeric(x[,c(3,5)]))
  }else{
    r <- prod(as.numeric(x[,c(3,5)]))
  }
  e <- stri_replace_first_fixed(e, x[,2], r)
  e <- stri_replace_first_regex(e, '\\((\\d+)\\)', "$1")
  return(e)
}

calculate2('5 * 9 * (7 * 3 * 3 + 9 * 3) + (8 + 6 * 4) * 8')
calculate2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340
print(sum(sapply(input, calculate2)), 22)

sum(c(51, 46, 1445,669060,23340))

e <- calculateOnce(e); e

compute <- function(x, add_first = FALSE) {
  `%+%` <- `+`; `%*%` <- `*`
  if (!add_first) x <- gsub("(\\*)", "%*%", x)
  print(sum(sapply(gsub("(\\+)", "%+%", x), function(x) eval(parse(text = x)))), 22)
}
compute(input)
compute(input, TRUE)
