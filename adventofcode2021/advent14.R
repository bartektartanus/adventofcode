require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input14.txt")

t = input[1]
sub = stri_match_first_regex(input[-(1:2)], "(..) -> (.)")[,-1]
sub[,2] = stri_paste(stri_sub(sub[,1], 1,1), sub[,2])

temp = t
steps=10
for(s in 1:steps) {
  print(s)
  newTemp = character(stri_length(temp))
  for(i in seq_len(stri_length(temp)-1)) {
    newTemp[i] = sub[sub[,1] == stri_sub(temp, i, i+1), 2]
  }
  newTemp[stri_length(temp)] = stri_sub(temp, -1,-1)
  temp = paste0(newTemp, collapse="")
}
temp

tab = sort(table(stri_sub(temp, 1:stri_length(temp), len=1)))
tail(tab, 1) - head(tab, 1)

## part 2


t = input[1]
sub = stri_match_first_regex(input[-(1:2)], "(..) -> (.)")[,-1]
sub = cbind(sub[,1], stri_paste(stri_sub(sub[,1], 1,1), sub[,2]), stri_paste(sub[,2], stri_sub(sub[,1], 2,2)))
sub

counts = rep(0, nrow(sub))
names(counts) = sub[,1]

counts[stri_sub(t, 1:(stri_length(t)-1), len=2)] = 1
counts

steps = 40
for(s in 1:steps) {
  print(s)
  newCounts = counts
  newCounts[TRUE] = 0
  for(i in seq_len(nrow(sub))) {
    newCounts[sub[i,2]] = newCounts[sub[i,2]] + counts[sub[i,1]]
    newCounts[sub[i,3]] = newCounts[sub[i,3]] + counts[sub[i,1]]
  }
  counts = newCounts 
}
sort(counts)
c1 = c(counts, counts)
names(c1) = c(stri_sub(names(counts),1,1), stri_sub(names(counts),2,2))
x = aggregate(c1, list(names(c1)), sum)
format(diff(x[order(x[,2])[c(1, nrow(x))],2])/2, digits=22)
