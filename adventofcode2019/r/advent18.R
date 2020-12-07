require(stringi)
require(compiler)
require(microbenchmark)
read.fwf

readMap <- function(file, ncol){
  matrix(unlist(read.fwf(file, widths=rep(1,ncol), sep="1", comment.char="", stringsAsFactors=FALSE)), ncol=ncol)
}

map <- readMap("~/adventofcode2019/input18.txt",81)
map <- readMap("~/adventofcode2019/input18a.txt",81)
mapTest1 <- readMap("~/adventofcode2019/input18-test1.txt",9)
mapTest3 <- readMap("~/adventofcode2019/input18-test3.txt",24)
mapTest4 <- readMap("~/adventofcode2019/input18-test4.txt",17)
mapTest5 <- readMap("~/adventofcode2019/input18-test5.txt",24)
map <- mapTest1

keys = stri_detect_fixed(stri_paste(map, collapse = ""), letters)
keys

cache <- list()

distanceToKey <- function(map){
  rows <- nrow(map)
  cols <- ncol(map)
  keys = stri_detect_fixed(stri_paste(map, collapse = ""), letters)
  #letters[keys]
  startingCol <- which(colSums(map == "@") > 0)
  startingRow <- which(rowSums(map == "@") > 0)
  cacheKey <- stri_paste(startingRow,",",startingCol,stri_paste(sort(letters[keys]),collapse=""))
  fromCache <- cache[[cacheKey]]
  if(!is.null(fromCache)){
    cacheCount <<- cacheCount+1
    if(cacheCount %% 10000 == 0){
      cat("\ncache hit, size=",length(cache), "count=", cacheCount,"keyLen=", sum(keys)) 
    }
    return(fromCache)
  }
  distance <- matrix(Inf, ncol = cols, nrow=rows)
  prevDistance = distance
  distance[startingRow, startingCol] = 0
  while(any(prevDistance != distance)){
    prevDistance = distance
    distance[-rows, ] = pmin(distance[-1,] + 1, distance[-rows,])
    distance[-1, ] = pmin(distance[-rows,] + 1, distance[-1,])
    distance[, -cols] = pmin(distance[,-1] + 1, distance[,-cols])
    distance[,-1] = pmin(distance[,-cols] + 1, distance[,-1])
    distance[map == "#"] = Inf
    for(k in LETTERS[keys]){
      distance[map == k] = Inf
    }
  }
  distance
  result <- rep(0, sum(keys))
  names(result) <- letters[keys]
  for(k in letters[keys]){
    result[k] <- distance[map == k]
  }
  cache[[cacheKey]] <<- result
  result
}

distanceToKey2 <- function(map){
  keys = stri_detect_fixed(stri_paste(map, collapse = ""), letters)
  #letters[keys]
  startingCol <- which(colSums(map == "@") > 0)
  startingRow <- which(rowSums(map == "@") > 0)
  cacheKey <- stri_paste(startingRow,",",startingCol,stri_paste(sort(letters[keys]),collapse=""))
  fromCache <- cache[[cacheKey]]
  if(!is.null(fromCache)){
    cacheCount <<- cacheCount+1
    if(cacheCount %% 10000 == 0){
      cat("\ncache hit, size=",length(cache), "count=", cacheCount,"keyLen=", sum(keys)) 
    }
    return(fromCache)
  }
  distance <- matrix(Inf, ncol = cols, nrow=rows)
  prevDistance = distance
  distance[startingRow, startingCol] = 0
  # while(any(prevDistance != distance)){
    # prevDistance = distance
  p <- c(startingRow, startingCol)
  if(any(map[startingRow, startingCol+1] == valid)){
    distance <- populateDistance(map, distance, p, 0:1)    
  }
  if(any(map[startingRow, startingCol-1] == valid)){
    distance <- populateDistance(map, distance, p, 0:-1)  
  }
  if(any(map[startingRow+1, startingCol] == valid)){
    distance <- populateDistance(map, distance, p, 1:0)
  }
  if(any(map[startingRow-1, startingCol] == valid)){
    distance <- populateDistance(map, distance, p, -1:0)
  }
  # }
  distance
  result <- rep(0, sum(keys))
  names(result) <- letters[keys]
  for(k in letters[keys]){
    result[k] <- distance[map == k]
  }
  cache[[cacheKey]] <<- result
  result
}

populateDistance <- function(map, currentDistance, p, d){
  p <- p+d
  currentDistance[p[1], p[2]] <- 
    min(currentDistance[p[1],p[2]], currentDistance[p[1] - d[1], p[2] - d[2]] + 1)
  cd <- currentDistance[p[1], p[2]] 
  if(currentDistance[p[1] + d[1], p[2] + d[2]] > cd+1 && any(map[p[1] + d[1], p[2] + d[2]] == valid)){
    currentDistance <- populateDistance(map, currentDistance, p, d)
  }
  if(currentDistance[p[1] + d[2], p[2] + d[1]] > cd+1 && any(map[p[1] + d[2], p[2] + d[1]] == valid)){
    currentDistance <- populateDistance(map, currentDistance, p, rev(d))
  }
  if(currentDistance[p[1] - d[2], p[2] - d[1]] > cd+1 && any(map[p[1] - d[2], p[2] - d[1]] == valid)){
    currentDistance <- populateDistance(map, currentDistance, p , -rev(d))
  }
  currentDistance
}



distanceToKey <- cmpfun(distanceToKey)

allKeysCollected <- function(map){
  all(stri_detect_fixed(stri_paste(map, collapse = ""), letters) == FALSE)
}

findPath <- function(map, currentPath = list(keys=c(), len=0)){
  #cat("\n")
  #print(map)
  #print(currentPath)
  if(allKeysCollected(map)){
    if(currentPath$len < globalShortestPath){
      globalShortestPath <<- currentPath$len
      cat("\nlen=",globalShortestPath," path=",currentPath$keys,"\n")
    }
    return(currentPath)
  }  
  distance <- distanceToKey2(map)
  distance <- sort(distance)
  shortestPath <- list(keys=c(),len=Inf)
  for(i in seq_along(distance)){
    #cat("\ndist=\n")
    #print(distance)
    if(is.finite(distance[i])){
      key <- names(distance[i])
      nextMap <- map
      nextMap[nextMap == "@"] <- "."
      nextMap[nextMap == key] <- "@"
      nextMap[nextMap == toupper(key)] <- "."
    
      nextPath <- list(keys = c(currentPath$keys, key), len=currentPath$len + unname(distance[i]))
      if(nextPath$len <= globalShortestPath){
        r <- findPath(nextMap, nextPath)
        if(r$len < shortestPath$len){
          #cat("\n  shortest=",shortestPath$len, " new=", r$len)
          shortestPath <- r
        }
      }else{
        skipped <<- skipped + 1
        if(skipped %% 100000 == 0){
          cat("\nskipped",skipped, "current=",currentPath$keys, "dropDepth=",length(distance)-1,setdiff(names(distance),key),"nextLen=",nextPath$len,"global=",globalShortestPath)  
        }
        
      }
      
    }
  }
  shortestPath
}
valid <- c(".", letters)
cache <<- list(); cacheCount = 0; globalShortestPath <<- Inf; skipped <<- 0; rows <- nrow(map);cols <- ncol(map)
findPath(mapTest4)
findPath(map)

Rprof()
findPath(mapTest4)
distanceToKey2(map)
Rprof(NULL)
summaryRprof()

stri_paste(nextMap, sep="")
cat(stri_paste(apply(map,1,function(x) stri_paste(x, collapse="")), collapse="\n"))


removeDeadEnds <- function(map){
  rows <- nrow(map)
  cols <- ncol(map)
  anythingChanged = TRUE
  while(anythingChanged){
    anythingChanged = FALSE
    for(i in 2:(rows-1)){
      for(j in 2:(cols-1)){
        if((map[i,j] == "." || any(map[i,j] == LETTERS)) 
           && sum(c(map[i-1,j], map[i+1,j], map[i,j-1], map[i,j+1])=="#")==3){
          map[i,j] <- "#"
          anythingChanged = TRUE
        }
      }
    }
  }
  map
}

map <- removeDeadEnds(map)

microbenchmark(distanceToKey(map), distanceToKey2(map))

m <- matrix(0, 1000, 1000)
test1 <- function(m){m[50,50] <- 1;m}
test2 <- function(m){m <- test1(m); m}
microbenchmark(test1(m), test2(m))
