require("stringi")
require(animation)
input <- readLines("input09.txt")
inputTest = stri_split_lines("7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")[[1]]

part1 = function(input) {
  x = matrix(as.integer(stri_split_fixed(input,",",n=2, simplify = TRUE)),ncol=2)
  s = 0
  for(i in 2:(nrow(x))) {
    for(j in 1:(i-1)) {
      r = prod(abs(x[i,]-x[j,])+1)
      s = max(s, r)
    }
  }
  s
}

part1(inputTest) == 50
part1(input)

part2 = function(input) {
  x = matrix(as.integer(stri_split_fixed(input,",",n=2, simplify = TRUE)),ncol=2)
  s = 0
  xp = x[251,1]
  yp = min(x[x[,1] >= x[250,1] & (1:496)>250,2])
  xr = x[248,1]
  yr = max(x[x[,1] >= x[249,1] & (1:496)<249,2])
  a=0
  for(i in 1:(nrow(x)/4)) {
    if(xp <= x[250+i,1] && yp <= x[250+i,2]) {
      p = prod(abs(x[250+i,]-x[250,])+1)
      if(p > s) {
        s = p
        a = (i+250) 
      }
    }
    if(xr <= x[249-i,1] && yr >= x[249-i, 2]) {
      r = prod(abs(x[249-i,]-x[249,])+1)
      if(r > s) {
        s = r
        a = (249-i)
      }

    }
    xp = max(xp, x[250+i,1])
    xr = max(xr, x[249-i,1])
  }
  s
}
part2(inputTest)
Rprof(tmp <- tempfile())
part2(input)
Rprof(NULL)
summaryRprof(tmp)

part2plot = function(input) {
  x = matrix(as.integer(stri_split_fixed(input,",",n=2, simplify = TRUE)),ncol=2)
  s = 0
  xp = x[251,1]
  yp = min(x[x[,1] >= x[250,1] & (1:496)>250,2])
  xr = x[248,1]
  yr = max(x[x[,1] >= x[249,1] & (1:496)<249,2])
  a=0
  ani.record(reset = TRUE)
  for(i in 1:(nrow(x)/4)) {
    if(xp <= x[250+i,1] && yp <= x[250+i,2]) {
      p = prod(abs(x[250+i,]-x[250,])+1)
      plot(x, pch=NA)
      lines(x, col="blue", pch=NULL)
      points(x[250+i,1], x[250+i,2])
      rect(x[250+i,1], x[250+i,2], x[250,1], x[250,2]) 
      text(35000, mean(75000), paste0("Max:",s), cex=1.5)
      text(65000, mean(75000), paste0("Current:",p), cex=1.5)
      ani.record()
      if(p > s) {
        s = p
        a = (i+250) 
      }
    }
    if(xr <= x[249-i,1] && yr >= x[249-i, 2]) {
      r = prod(abs(x[249-i,]-x[249,])+1)
      plot(x, pch=NA)
      lines(x, col="blue", pch=NULL)
      points(x[249-i,1], x[249-i,2])
      rect(x[249-i,1], x[249-i,2], x[249,1], x[249,2]) 
      text(35000, mean(75000), paste0("Max:",s), cex=1.5)
      text(65000, mean(75000), paste0("Current:",r), cex=1.5)
      ani.record()
      if(r > s) {
        s = r
        a = (249-i)
      }
      
    }
    xp = max(xp, x[250+i,1])
    xr = max(xr, x[249-i,1])
  }
  if(a > 250) {
    rect(x[a,1], x[a,2], x[250,1], x[250,2], col="red")  
  } else {
    rect(x[a,1], x[a,2], x[249,1], x[249,2], col="red")
  }
  
  #points(x, col="black", pch=20)
  if(a > 250) {
    points(x[c(250,a),], col="green") 
  } else {
    points(x[c(249,a),], col="green")
  }
  ani.record()
  saveGIF(ani.replay(), movie.name = "advent09.gif", interval = 0.8, ani.width=800, ani.height=800)
  s
}

part2plot(input)
