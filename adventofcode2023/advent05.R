require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input05.txt")
testInput = stri_split_fixed("seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4","\n")[[1]]


part1 = function(x) {
  a = stri_split_fixed(stri_join(x, collapse="\n"), "\n\n")[[1]]
  
  seeds = as.double(stri_match_all_regex(a[1], "\\d+")[[1]])
  seeds
  steps = a[-1]
  
  locations = c()
  for(s in seeds) {
    cs = s
    for(step in steps) {
      m = apply(stri_extract_all_regex(stri_split_fixed(step, "\n")[[1]][-1], "\\d+", simplify=TRUE), 2, as.double)
      cs = nextStep(cs, m)
    }
    locations = c(locations, cs)
  }
  min(locations)
}

nextStep = function(cs, m) {
  for(i in seq_len(nrow(m))){
    if(cs >= m[i,2] && cs < (m[i,2] + m[i,3])) {
      return(cs - diff(m[i,1:2]))
    }
  }
  return(cs)
}

part1(testInput)
part1(input)

sum(seeds[c(FALSE,TRUE)])



BiocManager::install("IRanges")
require(IRanges)

x <- IRanges(start=seeds[c(T,F)], width=seeds[c(F,T)])
