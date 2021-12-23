input <- c(8,0,17,4,1,12)
input <- c(0,3,6)
require(stringi)
require(stringr)
require(bit64)

lastTurn <- 1:length(input)

lastNumber <- tail(input, 1)
turn <- length(lastTurn)+1
while(turn < 2020){
    
  turn <- turn + 1
}