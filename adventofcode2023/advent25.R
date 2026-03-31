require(stringi)
require(sna)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input25.txt")
testInput = stri_split_fixed("jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr","\n")[[1]]

part1 = function(x) {
  a = unique(unlist(stri_extract_all_regex(x, "\\w{3}")))
  n = length(a)
  m = matrix(FALSE, n, n, dimnames = list(a, a))
  for(i in x) {
    h = stri_extract_all_regex(i, "\\w{3}")[[1]]
    for(k in 2:length(h)) {
      m[h[1], h[k]] = TRUE
      m[h[k], h[1]] = TRUE
    }
  }
  s = rowSums(m)
  s
}

part1(testInput)
