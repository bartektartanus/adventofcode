input <- readLines("~/workspace-private/adventofcode2020/input01.txt")
input <- as.integer(input)

partOne <- function() {
  for (i in input) {
    for (j in input) {
      if (i != j && i + j == 2020) {
        return(i * j)
      }
    }
  }
}
partOne()

partTwo <- function(){
  for(i in input){
    for(j in input){
      for(k in input){
        if(i + j + k == 2020){
          return(i * j * k)
        } 
      }
    }
  }
}

partTwo()
