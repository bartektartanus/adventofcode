require(stringi)
require(microbenchmark)

basePattern <- c(0, 1, 0, -1)
testSignal <- as.integer(stri_sub("12345678", 1:8, len = 1))

fftPhase <- function(signal, pattern){
  signalLength = length(signal)
  result = rep(0, signalLength)
  for(i in seq_len(signalLength)){
    #cat(" ",i)
    result[i] <- abs(sum(signal * rep(basePattern, each=i, length=signalLength+1)[-1]))%%10
  }
  result
}


fftPhase2 <- function(signal, pattern){
  signalLength = length(signal)
  result = rep(0, signalLength)
  for(i in seq_len(signalLength/3)){
    
    result[i] <- abs(sum(signal * rep(basePattern, each=i, length=signalLength+1)[-1]))%%10
    # for(j in 1:i){
    #   if(3*i+j-1 <= signalLength){
    #     result[i] <- result[i] + sum(signal[seq(i+j-1,signalLength, by = 4*i)]) -
    #       sum(signal[seq(3*i+j-1, signalLength, by = 4*i)])  
    #   }else{
    #     result[i] <- result[i] + sum(signal[seq(i+j-1,signalLength, by = 4*i)])
    #   }
    #   
    #   #cat("\ni=",i," j=",j, result[i])
    # }
    # result[i] <- abs(result[i])%%10
  }
  cat(i)
  lastIndex <- i+1
  result[i+1] <- sum(signal[seq(i+1, len = i+1)]) %% 10
  lastValue <- result[i+1]
  for(i in seq(lastIndex+1, signalLength/2)){
    #cat("\n2 i=",i)
    # result[i] <- sum(signal[seq(i, len = i)]) %% 10
    lastValue <- lastValue - sum(signal[lastIndex:(i-1)]) + sum(signal[2*i - (2:1)], na.rm = TRUE)
    result[i] <- abs(lastValue)%%10
  }
  indexes <- rev(signalLength/2 + seq_len(signalLength/2))
  result[indexes] <- cumsum(signal[indexes])%%10
  result
}
signal <- c(1,2,3,4,5,6,7,8)
fftPhase(signal, basePattern)[211:220]
fftPhase2(signal, basePattern)[211:220]
mean(fftPhase(signal, basePattern) == fftPhase2(signal, basePattern))
microbenchmark(fftPhase(signal, basePattern), fftPhase2(signal, basePattern), times=1)

fftPhase(testSignal, basePattern)

fft <- function(rawSignal, pattern, n){
  signal <- as.integer(stri_sub(rawSignal, 1:stri_length(rawSignal), len=1))
  for(i in seq_len(n)){
    cat("\nphase=",i," ")
    signal <- fftPhase2(signal, pattern)
  }
  stri_join(signal[1:8], collapse="")
}

fft("12345678", basePattern, 4)
fft("80871224585914546619083218645595", basePattern, 100)

rawSignal <- readLines("~/adventofcode2019/input16.txt")[1]
rawSignal <- stri_dup(rawSignal, 10000)

fft(rawSignal, basePattern, 100)
fft(stri_dup(rawSignal, 10000), basePattern, 100)

n <- 8
patternMatrix <- matrix(0, n, n)
for(i in 1:n){
  patternMatrix[i,] <- rep(basePattern, each = i, length = n+1)[-1]
}
patternMatrix


fft2 <- function(rawSignal, pattern, n){
  signal <- as.integer(stri_sub(rawSignal, 1:stri_length(rawSignal), len=1))
  
  offset <- sum(signal[1:7] * 10^(6:0) )
  input <- signal[(offset + 1):length(signal)]
  
  rev_input <- rev(input)
  for(i in 1:100){
    cat(i," ")
    rev_input <- cumsum(rev_input) %% 10
  }
  
  stri_paste(rev(rev_input)[1:8],collapse = "")
}

fft2(rawSignal, basePattern, 100)
