input <- readLines('~/workspace-private/adventofcode2020/input07.txt')

require(stringi)

x <- stri_match_first_regex(input, '([a-z ]+?) bags contain (.*)\\.$')[,-1]

l <- list()
for(i in seq_len(nrow(x))){
  l[[x[i,1]]] <- stri_match_first_regex(stri_split_fixed(x[i,2], ', ')[[1]], '\\d+ (.*?) bags?')[,2]
}
l[1:3]

l['shiny gold']

bags <- character(0)
newBags <- c('shiny gold')
while(length(bags) != length(newBags)){
  bags <- newBags
  for(i in seq_len(length(l))){
    b <- names(l[i])
    if(all(newBags != b)){
      x <- intersect(newBags, l[[i]])
      if(length(x) > 0) {
        newBags <- c(newBags, b)
        print(length(newBags))
      }
    }
  }
}
# part 1
length(unique(newBags)) - 1

input <- stri_split_fixed("shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.",'\n')[[1]]

x <- stri_match_first_regex(input, '([a-z ]+?) bags contain (.*)\\.$')[,-1]

l <- list()
for(i in seq_len(nrow(x))){
  l[[x[i,1]]] <- stri_split_fixed(x[i,2], ', ')[[1]]
}
l[1:3]
t <- list()
countBags <- function(b){
  if(!is.integer(t[[b]])){
    r <- 1
    for(bag in l[[b]]){
      if(bag != 'no other bags'){
        e <- stri_match_first_regex(bag, '(\\d+) ([a-z ]+) bag')
        r <- r + as.integer(e[,2]) * countBags(e[,3]) 
      }
    }
    t[[b]] <<- r
    return(r)
  } else {
    return(t[[b]])
  }
}
countBags('shiny gold') - 1
