require("stringi")
input <- readLines("input07.txt")
inputTest = stri_split_lines(".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")[[1]]

map = function(input) {
  s = 0
  input = stri_replace_first_fixed(input, "S", "|")
  for(i in 2:length(input)) {
    b = stri_locate_all_fixed(input[i-1], "|")[[1]][,1]
    for(r in b) {
      x = stri_sub(input[i], r,r)
      if(x == ".") {
        stri_sub(input[i], r, r) = "|"
      } else if(x == "^") {
        stri_sub(input[i], r-1, r+1) = "|^|"
        s = s + 1
      }
    }
  }
  list(map=input, s=s)
}

part1 = function(input) {
  map(input)$s
}

part1(inputTest) == 21
part1(input)

dfs <- function(r, i, m) {
  key <- stri_paste(r, ":", i)
  if (exists(key, envir = cache)) {
    return(cache[[key]])
  }
  s = 1
  if (r < length(m)) {
    if (stri_sub(m[r], i, i) == "^") {
      s = dfs(r + 1, i - 1, m) + dfs(r + 1, i + 1, m)
    } else {
      s = dfs(r + 1, i, m)
    }
  }
  
  cache[[key]] = s
  s
}


part2 = function(input) {
  m = map(input)$map
  
  i <- stri_locate_first_fixed(m[1], "|")[,1]
  
  dfs(2, i, m)
}

cache <- new.env(hash = TRUE)
part2(inputTest)

cache <- new.env(hash = TRUE)
format(part2(input), digits=22)

