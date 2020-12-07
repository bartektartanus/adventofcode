input <- readLines("~/workspace-private/adventofcode2015/input07.txt")

require(stringi)

# part 1
l <- list()
for(i in input){
  x <- stri_match_last_regex(i, "(.+) -> (.+)$")[1,]
  l[[x[3]]] <- x[2]
}

calc <- function(x){
  if(is.finite(as.integer(x))){
    return(as.integer(x))
  }
  v <- l[[x]][1]
  if(stri_detect_regex(v, 'AND|OR|LSHIFT|RSHIFT')){
    p <- stri_match_first_regex(v, '(.*) (AND|OR|LSHIFT|RSHIFT) (.*)')[,-1]
    a <- as.integer(calc(p[1]))
    b <- as.integer(calc(p[3]))
    op <- switch(p[2], AND=bitwAnd, OR=bitwOr, LSHIFT=bitwShiftL, RSHIFT=bitwShiftR)
    l[[x]][1] <<- op(a,b)
  }else if(stri_detect_regex(v, '^NOT')){
    p <- stri_match_first_regex(v, 'NOT (.*)')[,-1]
    l[[x]][1] <<- bitwNot(as.integer(calc(p)))
  }else if(stri_detect_regex(v, '^[a-z]+$')){
    l[[x]][1] <<- calc(v)
  }else{
    l[[x]][1] <<- v
  }
  return(l[[x]][1])
}

calc('a')

# part 2
l <- list()
for(i in input){
  x <- stri_match_last_regex(i, "(.+) -> (.+)$")[1,]
  l[[x[3]]] <- x[2:1]
}

l[['b']][1] <- 46065
calc('a')
