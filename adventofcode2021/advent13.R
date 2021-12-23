require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input13.txt")

dots = matrix(as.integer(stri_split_fixed(input[1:902], ",", n = 2,simplify = TRUE)), ncol = 2)[,2:1] + 1
folds = stri_match_first_regex(tail(input, 12), "fold along ([xy])=(\\d+)")[,-1]

m = matrix(FALSE, nrow=max(dots[,1]), ncol = max(dots[,2]))
for(r in seq_len(nrow(dots))) {
  m[dots[r,1], dots[r,2]] = TRUE
}


# patr 1
sum(m[,1:(as.integer(folds[1,2]))] | m[,ncol(m):(as.integer(folds[1,2])+2)])

# part 2
t = m
for(i in seq_len(nrow(folds))) {
  
  if(folds[i,1] == "y") {
    t = t(t)
  }
  fx = as.integer(folds[i,2]) + 1
  fold_span = ncol(t)- fx
  t[,(fx-fold_span):(fx-1)] = t[,(fx-fold_span):(fx-1)] | t[,(fx+fold_span):(fx+1)]
  t <- t[,1:(fx-1)]
  if(folds[i,1] == "y") {
    t = t(t)
  }
}

dim(t)
matrix(c(" ", "#")[t+1], nrow=nrow(t))
