input <- readLines("~/workspace-private/adventofcode2015/input06.txt")

require(stringi)

# part 1
m <- matrix(FALSE, 1000, 1000)
for(i in input){
  x <- as.integer(stri_match_first_regex(i, '(\\d+),(\\d+) through (\\d+),(\\d+)')[,-1])+1
  if(stri_startswith_fixed(i, 'turn on')){
    m[x[1]:x[3], x[2]:x[4]] <- TRUE
  }else if(stri_startswith_fixed(i, 'turn off')){
    m[x[1]:x[3], x[2]:x[4]] <- FALSE
  }else{
    m[x[1]:x[3], x[2]:x[4]] <- !m[x[1]:x[3], x[2]:x[4]]
  }
}
sum(m)
input <- c('turn on 0,0 through 0,0', 'toggle 0,0 through 999,999')
# part 2
m <- matrix(0, 1000, 1000)
for(i in input){
  x <- as.integer(stri_match_first_regex(i, '(\\d+),(\\d+) through (\\d+),(\\d+)')[,-1])+1
  if(stri_startswith_fixed(i, 'turn on')){
    m[x[1]:x[3], x[2]:x[4]] <- m[x[1]:x[3], x[2]:x[4]] + 1
  }else if(stri_startswith_fixed(i, 'turn off')){
    m[x[1]:x[3], x[2]:x[4]] <- pmax(m[x[1]:x[3], x[2]:x[4]]-1, 0)
  }else{
    m[x[1]:x[3], x[2]:x[4]] <- m[x[1]:x[3], x[2]:x[4]] + 2
  }
}
sum(m)
