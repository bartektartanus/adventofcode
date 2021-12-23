input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input21.txt')
input <- c('mxmxvkd kfcds sqjhc nhms (contains dairy, fish)',
           'trh fvjkl sbzzf mxmxvkd (contains dairy)',
           'sqjhc fvjkl (contains soy)',
           'sqjhc mxmxvkd sbzzf (contains fish)')
require(stringi)

x <- stri_match_first_regex(input, "^([a-z ]+) \\(contains ([a-z, ]+)+\\)$")
x

allAllergens <- unique(unlist(stri_split_fixed(x[,3],', ')))
allFood <- unique(unlist(stri_split_fixed(x[,2],' ')))
r <- list()
for(i in allAllergens){
  r[[i]] <- allFood
}

for (i in seq_len(nrow(x))) {
  food = stri_split_fixed(x[i,2], ' ')[[1]]
  for (allergen in stri_split_fixed(x[i,3], ', ')[[1]]) {
    for (f in allFood) {
      if (!(f %in% food)) { 
        r[[allergen]] <- setdiff(r[[allergen]], f) 
      }
    }
  }
}

setdiff(allFood, unique(unlist(r)))

nonAllergic <- setdiff(allFood, unique(unlist(r)))

allergic <- unique(unlist(r))

sum(sapply(stri_split_fixed(x[,2], ' '), length)) - sum(sapply(stri_split_fixed(x[,2], ' '), function(x) sum(allergic %in% x)))

r

while(any(sapply(r, length) > 1)){
  for(i in seq_len(length(r))){
    if(length(r[[i]]) == 1) {
      for(a in seq_len(length(r))){
        if(a != i){
          r[[a]] <- setdiff(r[[a]], r[[i]])
        }
      }
    }
  }
}
r

stri_paste(unlist(r)[order(names(r))], collapse=',')
