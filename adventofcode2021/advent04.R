require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input04.txt")

numbers <- as.integer(stri_split_fixed(input[1],",")[[1]])

boards <- list()
i = 1
b = input[-(1:2)]
while(length(b) >= 5) {
  boards[[i]] <- list(b=matrix(as.integer(unlist(stri_split_regex(b[1:5], "\\s+", omit_empty = TRUE))),5,5,byrow=TRUE), h=matrix(FALSE, 5, 5))
  b <- b[-(1:6)]
  i = i+1
}

bingo <- function() {
  for(n in numbers) {
    for(i in seq_along(boards)) {
      b = boards[[i]]$b
      h = boards[[i]]$h
      if(any(b == n)){
        boards[[i]]$h = boards[[i]]$h | (b == n)
        h = boards[[i]]$h
        rs = which(rowSums(h) == 5)
        cs = which(colSums(h) == 5)
        if(length(rs) > 0) {
          print("bingo")
          print(b)
          print(h)
          print(sum(b[rs,]) * n)
          print(sum(b[!h]) * n)
          return()
        }
        if(length(cs) > 0) {
          print("bingo")
          print(b)
          print(h)
          print(sum(b[,cs]) * n)
          print(sum(b[!h]) * n)
          return()
        }
      }
    }
  }
}

bingo()


bingo2 <- function() {
  winners <- c()
  for(n in numbers) {
    for(i in setdiff(seq_along(boards), winners)) {
      b = boards[[i]]$b
      h = boards[[i]]$h
      if(any(b == n)){
        boards[[i]]$h = boards[[i]]$h | (b == n)
        h = boards[[i]]$h
        rs = which(rowSums(h) == 5)
        cs = which(colSums(h) == 5)
        if(length(rs) > 0) {
          print("bingo")
          print(b)
          print(h)
          print(sum(b[rs,]) * n)
          print(sum(b[!h]) * n)
          winners = c(winners, i)
        }
        if(length(cs) > 0) {
          print("bingo")
          print(b)
          print(h)
          print(sum(b[,cs]) * n)
          print(sum(b[!h]) * n)
          winners = c(winners, i)
        }
      }
    }
  }
  print(winners)
}

bingo2()
