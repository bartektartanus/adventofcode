require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input04.txt")
testInput = stri_split_fixed("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11","\n")[[1]]

matches = function(y) {
  a = stri_extract_all_regex(y[1], "\\d+")[[1]]
  b = stri_extract_all_regex(y[2], "\\d+")[[1]]
  l = sum(b %in% a)
}

part1 = function(x){
  x = stri_replace_first_regex(x, "^Card +\\d+:","")
  sum(sapply(stri_split_fixed(x, "|"), function(y){
    l = matches(y)
    if(l > 0) {
      2^(l-1)
    } else {
      0
    }
  }))
}


part1(testInput)
part1(input)

part2 = function(x){
  x = stri_replace_first_regex(x, "^Card +\\d+:","")
  cards = stri_split_fixed(x, "|")
  cardsTotal = rep(1, length(cards))
  for(i in seq_along(cards)){
    y = cards[[i]]
    l = matches(y)
    if(l > 0) {
      cardsTotal[i + 1:l] = cardsTotal[i + 1:l] + cardsTotal[i]
    }
  }
  sum(cardsTotal)
}
part2(testInput)
part2(input)
