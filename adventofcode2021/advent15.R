require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input15.txt")
input = stri_split_lines1("1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
x = matrix(as.integer(stri_extract_all_regex(input, "(\\d)", simplify=TRUE)), length(input))
x

path = function(a) {
  n = nrow(a)
  cost = matrix(NA, nrow=n+2, ncol=n+2)
  cost[n + 1, n + 1] = a[n,n]
  prev = cost
  
  ind = 1:n + 1
  while(any(is.na(cost[ind, ind])) | any(prev != cost, na.rm = TRUE)){
    print(sum(is.na(cost[ind,ind])))
    prev = cost
    cost[ind, ind] = pmin(cost[ind, ind], a + pmin(cost[ind-1,ind], cost[ind+1, ind], cost[ind, ind-1], cost[ind, ind+1], na.rm = TRUE), na.rm=TRUE)
  }
  
  cost[2,2] - a[1,1]
}

path(x)

y = x
for(i in 1:4) {
  n = ncol(y)
  y = cbind(y, y[, (n+1-ncol(x)):n] %% 9 + 1)
}
for(i in 1:4) {
  n = nrow(y)
  y = rbind(y, y[(n+1-nrow(x)):n,] %% 9 + 1)
}

path(y)

