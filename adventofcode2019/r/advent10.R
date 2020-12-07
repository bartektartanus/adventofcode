require(stringi)

testMap <- as.matrix(read.fwf("~/adventofcode2019/input10-test.txt", rep(1,5), sep="1", comment.char=""))
testMap <- testMap == "#"


testMap2 <- as.matrix(read.fwf("~/adventofcode2019/input10-test2.txt", rep(1,20), sep="1", comment.char=""))
testMap2 <- testMap2 == "#"

map <- as.matrix(read.fwf("~/adventofcode2019/input10.txt", rep(1,36), sep="1", comment.char=""))
map <- map == "#"

findMax(testMap2)
findMax(map)

findMax <- function(map){
  maxAsteroids = 0
  location = c(0,0)
  for(i in seq_len(nrow(map))){
    for(j in seq_len(ncol(map))){
      if(map[i,j]){
        a = checkVisibleAsteroids(map, i,j)
        if(a >= maxAsteroids){
          maxAsteroids = a
          location = c(i,j)
          cat(a, location,"\n")
        }
      }
      
    }
  }
  print(location)
  maxAsteroids
}


checkVisibleAsteroids(testMap,1,1)

checkVisibleAsteroids <- function(map, x, y){
  count = 0
  for(i in seq_len(nrow(map))){
    for(j in seq_len(ncol(map))){
      if(map[i,j]){
        dx = x - i
        dy = y - j
        d = abs(gcd(dx, dy))
        if(d > 1){
          dx = dx/d
          dy = dy/d
          anyAsteroid = FALSE
          ax = i
          ay = j
          for(n in 1:(d-1)){
            ax = ax + dx
            ay = ay + dy
            # cat(x,y,i,j,ax, ay)
            if(map[ax, ay]){
              anyAsteroid = TRUE
              break
            }
          }
          if(!anyAsteroid){
            count = count + 1
          }
        }else if(d == 1){
          count = count + 1
        }
      }
    }
  }
  count
}
gcd(0,0)
gcd <- function(x, y) {
  while(y) {
    temp = y
    y = x %% y
    x = temp
  }
  return(x)
}

(180+atan(0.1)/pi * 360) %% 360

part2 <- function(map, x, y){
  
  asteroids <- data.frame(x=rep(0, sum(map) - 1), y=0, angle=0, round = 0)
  lastIndex = 1
  for(i in seq_len(nrow(map))){
    for(j in seq_len(ncol(map))){
      if(map[i,j] && !(i == x && j == y)){
        dx = x - i
        dy = y - j
        d = abs(gcd(dx, dy))
        count = 0
        if(d > 1){
          dx = dx/d
          dy = dy/d
          anyAsteroid = FALSE
          ax = i
          ay = j
          for(n in 1:(d-1)){
            ax = ax + dx
            ay = ay + dy
            if(map[ax, ay]){
              count = count + 1
            }
          }
        }
        asteroids[lastIndex, 1] = j - 1
        asteroids[lastIndex, 2] = i - 1
        asteroids[lastIndex, 4] = count
        asteroids[lastIndex, 3] = ((getAngle(dy, dx)/pi * 360) ) %% 360
        asteroids[lastIndex, 3] = getAngle3(c(i, -j),c(x,-y),c(x,-y+1))
        asteroids[lastIndex, 3] = (Arg(complex(real = dx, imaginary = -dy)) * 180 /pi ) %% 360
        lastIndex = lastIndex + 1
      }
      
      
    }
  }
  
  
  require(dplyr)
  asteroids %>% arrange(round, (angle))
}

findMax(map)
part2(map, 20, 24)
part2(testMap2, 14, 12)

## 11,12
## 12,1
## 12,2

getAngle <- function(x,y){
  if(x >= 0 && y <= 0){
    atan(x/y) 
  }else if(x >= 0 && y >= 0){
    atan(x/y + pi/2)
  }else if(x <= 0 && y >= 0){
    atan(x/y + pi)
  }else if(x <= 0 && y <= 0){
    atan(x/y + pi*3/2)
  }
}

atan2(2, 0) / pi * 360

A <- c(0,1); O <- c(0,0); B <- c(1,0)
acos((A-O) %*% (B-O)) * 180 / pi

getAngle2 <- function(a, b, c){
  acos((a-c) %*% (b-c)) * 180 / pi
}

getAngle3<-function(A, B, C){
  vector1=c(A[1]-B[1],A[2]-B[2])
  vector2=c(C[1]-B[1],C[2]-B[2])
  num=(vector1[1]*vector2[1]+vector1[2]*vector2[2])
  den=sqrt(vector1[1]^2+vector1[2]^2)*sqrt(vector2[1]^2+vector2[2]^2)
  angle=acos(num/den)
  angle=(360*angle)/(2*pi)
  return(angle)
}

# 3034z