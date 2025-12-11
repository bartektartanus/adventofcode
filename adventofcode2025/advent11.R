require("stringi")
input <- readLines("input11.txt")
inputTest = stri_split_lines("aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")[[1]]

inputTest2 = stri_split_lines("svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")[[1]]

count = function(l, n = "you", e = "out") {
  nn = l[[n]]
  s = 0
  for(a in nn) {
    if(a == e) {
      s = s + 1
    } else {
      s = s + count(l, a, e)
    }
  }
  s
}

part1 = function(input) {
  n = stri_match_first_regex(input, "(\\w+): ")[,2]
  l = stri_split_fixed(stri_match_first_regex(input, ": ([\\w ]+)$")[,2], " ")
  names(l) = n
  count(l)
}

part1(inputTest) == 5
part1(input)


count2 = function(l, n = "you", e = "out") {
  nn = l[[n]]
  if(any(nn==e)) {
    return(1)
  }
  s = 0
  for(a in nn) {
    key <- stri_paste(a, ":", e)
    if (exists(key, envir = cache)) {
      r = cache[[key]]
    } else {
      r = count2(l, a, e)  
      cache[[key]] = r
    }
    s = s + r
  }
  s
}

part2 = function(input) {
  n = stri_match_first_regex(input, "(\\w+): ")[,2]
  l = stri_split_fixed(stri_match_first_regex(input, ": ([\\w ]+)$")[,2], " ")
  names(l) = n
  prod(count2(l, "svr", "fft"), count2(l, "fft", "dac"), count2(l, "dac", "out")) + 
    prod(count2(l, "svr", "dac"), count2(l, "dac", "fft"), count2(l, "fft", "out"))
}

cache <- new.env(hash = TRUE)
part2(inputTest2) == 2
cache <- new.env(hash = TRUE)
format(part2(input), digits=22)

