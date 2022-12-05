require(stringi)
input <- sort(as.integer(readLines("~/workspace-private/adventofcode/adventofcode2015/input17.txt")),decreasing = TRUE)
inputTest <- c(20, 15, 10, 5, 5)

# part 1
f1 = function(input, total) {
  result = 0
  if(total == 0) {
    return(1)
  } else if(total > 0) {
    for(i in seq_along(input)) {
      newTotal = total - input[i]
      if(newTotal >= 0) {
        newInput = input[-(1:i)]
        result = result + f1(newInput, newTotal)
      }
    }
  } else {
    return(0)
  }
  result
}
f1(inputTest, 25)
f1(input, 150)

# part 2
f2 = function(input, total, count) {
  result = 0
  if(total == 0 && count == 4) {
    if(minCount > count) {
      minCount <<- count
      print(minCount)
    }
    return(1)
  } else if(total > 0) {
    for(i in seq_along(input)) {
      newTotal = total - input[i]
      if(newTotal >= 0) {
        newInput = input[-(1:i)]
        result = result + f2(newInput, newTotal, count + 1)
      }
    }
  } else {
    return(0)
  }
  result
}
minCount = 5
f2(inputTest, 25, 0)
minCount = 20
f2(input, 150, 0)
