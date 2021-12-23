input <- readLines('~/workspace-private/adventofcode/adventofcode2020/input22.txt')

require(stringi)

deck1 <- as.integer(input[2:((length(input)-1)/2)])
deck2 <- as.integer(input[((length(input)+5)/2):length(input)])

while(length(deck1) > 0 && length(deck2) > 0){
  if(deck1[1] > deck2[1]){
    deck1 <- c(deck1[-1], deck1[1], deck2[1])
    deck2 <- deck2[-1]
  }else{
    deck2 <- c(deck2[-1], deck2[1], deck1[1])
    deck1 <- deck1[-1]
  }
}

sum(deck1 * 50:1)
