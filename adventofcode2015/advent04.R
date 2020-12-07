input <- 'bgvyzdsv'

require(digest)
require(stringi)

md5 <- function(x){
  digest(x, algo="md5", serialize=F)
}

i <- 0
hash <- md5(paste0(input, i))
while(!stri_startswith_fixed(hash, '000000')){
  i <- i+1
  hash <- md5(paste0(input, i))
}
