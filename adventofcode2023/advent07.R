require(stringi)
input = read.table("~/workspace-private/adventofcode/adventofcode2023/input07.txt", col.names = c("hand", "bet"))
testInput = read.table(text="32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483", col.names=c("hand", "bet"))

eq = function(a,b) {
  length(a) == length(b) && all(a==b)
}


part1 = function(x) {
  cardOrder = function(a) {
    cards = rev(stri_split_fixed("A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2",", ")[[1]])
    o = 1:13
    names(o) = cards
    o[a]
  } 
  
  t = sapply(x$hand, function(x) {
    r = rle(sort(stri_sub(x, 1:5, 1:5)))
    l = sort(r$lengths,decreasing = TRUE)
    if(eq(l, 5)) {
      7
    } else if(eq(l, c(4,1))) {
      6
    } else if(eq(l, 3:2)) {
      5
    } else if(eq(l, c(3,1,1))) {
      4
    } else if(eq(l, c(2,2,1))) {
      3
    } else if(eq(l, c(2,1,1,1))) {
      2
    } else {
      1
    }
  })
  x = data.frame(x, t, 
                 h1=cardOrder(stri_sub(x$hand, 1, len=1)),
                 h2=cardOrder(stri_sub(x$hand, 2, len=1)),
                 h3=cardOrder(stri_sub(x$hand, 3, len=1)),
                 h4=cardOrder(stri_sub(x$hand, 4, len=1)),
                 h5=cardOrder(stri_sub(x$hand, 5, len=1))
                 )
  sum(x$bet[order(x$t, x$h1, x$h2, x$h3, x$h4, x$h5)] * 1:nrow(x))
}

part1(testInput)
part1(input)


part2 = function(x) {
  cardOrder = function(a) {
    cards = rev(stri_split_fixed("A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J",", ")[[1]])
    o = 1:13
    names(o) = cards
    o[a]
  } 
  
  t = sapply(x$hand, function(x) {
    r = rle(sort(stri_sub(x, 1:5, 1:5)))
    l = sort(r$lengths[r$values != "J"],decreasing = TRUE)
    if(all(r$values == "J")){
      l = 0
    }
    if(any(r$values == "J")) {
      l[1] = l[1] + r$lengths[r$values == "J"]  
    }
    
    if(eq(l, 5)) {
      7
    } else if(eq(l, c(4,1))) {
      6
    } else if(eq(l, 3:2)) {
      5
    } else if(eq(l, c(3,1,1))) {
      4
    } else if(eq(l, c(2,2,1))) {
      3
    } else if(eq(l, c(2,1,1,1))) {
      2
    } else {
      1
    }
  })
  x = data.frame(x, t, 
                 h1=cardOrder(stri_sub(x$hand, 1, len=1)),
                 h2=cardOrder(stri_sub(x$hand, 2, len=1)),
                 h3=cardOrder(stri_sub(x$hand, 3, len=1)),
                 h4=cardOrder(stri_sub(x$hand, 4, len=1)),
                 h5=cardOrder(stri_sub(x$hand, 5, len=1))
  )
  sum(x$bet[order(x$t, x$h1, x$h2, x$h3, x$h4, x$h5)] * 1:nrow(x))
}

part2(testInput)
part2(input)
