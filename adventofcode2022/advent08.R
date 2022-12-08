require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2022/input08.txt")
input = stri_split_fixed("30373
25512
65332
33549
35390", "\n")[[1]]

m = as.integer(t(simplify2array(stri_extract_all_regex(input, "\\d"))))
m = matrix(m, sqrt(length(m)))
m

s = 0
n = nrow(m)
for(i in 2:(n-1)) {
  for(j in 2:(n-1)) {
    v = m[i,j]
    if(all(m[1:(i-1), j] < v) || all(m[(i+1):n, j] < v) ||
       all(m[i, 1:(j-1)] < v) || all(m[i, (j+1):n] < v)) {
      s = s+1
    }
  }
}
s + (ncol(m) - 1) * 4

# part 2
s = 0
for(i in 2:(n-1)) {
  for(j in 2:(n-1)) {
    v = m[i,j]
    a = min(which((m[(i-1):1, j] - v) >=0), i-1)
    b = min(which((m[i, (j-1):1] - v) >=0), j-1)
    c = min(which((m[(i+1):n, j] - v) >=0), n-i)
    d = min(which((m[i, (j+1):n] - v) >=0), n-j)
    r = a * b * c * d
    if(r > s) {
      s = r
    }
  }
}
s
