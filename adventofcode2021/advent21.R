require(stringi)
require(collections)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input21.txt")


space = c(5,8)
score = c(0,0)
roll = 2
count = 0
while(all(score < 1000)){
  space = (space + c(roll, roll +3)*3 - 1) %% 10 + 1
  score = score + space
  roll = (roll + 5) %% 100 + 1
  count = count + 6
}
score
(score[2] - space[2]) * (count-3)

## part 2
space = c(5,8)

cache = dict()

part2 <- function(space, score){
  if(any(score >= 21)){
    return(score >= 21)
  }
  k <- paste(space, score, sep = ":", collapse=" ")
  if(cache$has(k)){
    return(cache$get(k))
  }
  
  r = c(0, 0)
  
  for(dice1 in 1:3){
    for(dice2 in 1:3){
      for(dice3 in 1:3){
        dice = sum(dice1,dice2,dice3)
        newSpace <- ((space[1] - 1) + dice) %% 10 + 1
        newScore <- score[1] + newSpace
        x <-  rev(part2(c(space[2], newSpace), c(score[2], newScore)))
        r <- r + x
      }
    }
  }
  cache$set(k, r)
  r
}
format(max(part2(1:2*4, c(0,0))), digits=22)
format(max(part2(space, c(0,0))), digits=22)
