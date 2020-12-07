require(stringi)
input <- as.integer(stri_split_fixed(readLines("~/adventofcode2019/input05.txt"), ",")[[1]])

intCode <- function(input){
  for(i in seq(1,length.out = length(input), by = 4)){
    action <- input[i]
    if(action == 1){
      input[input[i+3]+1] <- input[input[i+1]+1] + input[input[i+2]+1]
    }else if(action == 2){
      input[input[i+3]+1] <- input[input[i+1]+1] * input[input[i+2]+1]
    }else if(action == 99){
      break;
    }else{
      print("error")
    }
  }
  input[1]
}