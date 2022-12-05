input <- c(8,0,17,4,1,12)
input <- c(0,3,6)
require(stringi)
require(stringr)
require(r2r)

all = input
turn <- length(all)+1
last = all[turn-1]
while(turn <= 2020){
  x = which(all == last)
  n = length(x)
  if(n > 1) {
    last = x[n] - x[n-1]
  } else {
    last = 0
  }
  all = c(all, last)
  
  turn <- turn + 1
}
all[2020]

numberPosition = list("8"=1, "0"=2, "17"=3, "4"=4, "1"=5)
numberPosition = hashmap(list("8",1), "0"=2, "17"=3, "4"=4, "1"=5)
numberPosition = hashmap(list(8,1), list(0,2), list(17,3), list(4,4), list(1,5))
numberPosition = c()
numberPosition[8+1] = 1
numberPosition[0+1] = 2
numberPosition[17+1] = 3
numberPosition[4+1] = 4
numberPosition[1+1] = 5

currentNumber = input[length(input)]
currentPos = length(input)
while(currentPos < 3e7) {
  if(is.na(numberPosition[currentNumber+1])) {
    numberPosition[currentNumber+1] = currentPos
    currentNumber = 0;
  } else {
    newNumber = currentPos - numberPosition[currentNumber+1]
    numberPosition[currentNumber+1] = currentPos;
    currentNumber = newNumber
  }
  currentPos = currentPos + 1
  if(currentPos %% 1000000 == 0) {
    cat(currentPos, " ")
  }
}
print(currentNumber)
