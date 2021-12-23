require(stringi)
input <- "hepxcrrq"

alphabet <- setdiff(letters, c("i", "o", "l"))
allThreeGram <- stri_paste(letters[1:24], letters[2:25], letters[3:26])
allTwoGram <- stri_dup(letters, 2)

nextPassword <- function(input) {
  current <- stri_sub(input, 1:8, len=1)
  stri_paste(nextCandidate(current), collapse="")
}

nextCandidate <- function(current) {
  s = current
  while(TRUE) {
    s = increment(s)
    threeGram <- stri_paste(s[1:6], s[2:7], s[3:8])
    cond1 <- any(threeGram %in% allThreeGram)  
    twoGram <- stri_paste(s[1:7], s[2:8])
    cond3 <- sum(unique(twoGram) %in% allTwoGram) >= 2
    if(cond1 && cond3) {
      break
    } 
  }
  return(s)
}

increment <- function(s, i = 8) {
  w <- which(alphabet == s[i])
  if(s[i] == "z") {
    s[i] = "a"
    increment(s, i-1)
  } else {
    s[i] = alphabet[w+1]
    s
  }
}
increment(current)
nextPassword("abcdefgh")
nextPassword("hepxcrrq")
nextPassword("hepxxyzz")
