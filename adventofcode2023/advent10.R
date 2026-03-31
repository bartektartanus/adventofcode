require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2023/input10.txt")
testInput = stri_split_fixed("..F7.
.FJ|.
SJ.L7
|F--J
LJ...","\n")[[1]]

checkNode = function(map, distance, r,c,pipes) {
  r > 0 && r <= nrow(map) && c > 0 && c <= ncol(map) && (map[r,c] %in% pipes) && (distance[r,c] == -1)
}
checkNewNode = function(map, distance, r,c) {
  r > 0 && r <= nrow(map) && c > 0 && c <= ncol(map) && map[r,c] != "." && distance[r,c] == -1
}

nextStep = function(map, distance, nodes, i) {
  newNodes = list()
  for(n in nodes) {
    r = n[1]
    c = n[2]
    if(checkNode(map, distance, r+1, c, c("|", "L","J"))) {
      distance[r+1, c] = i
      nn = list("|"=c(r+2, c), "J"=c(r+1,c-1), "L"=c(r+1,c+1))[[map[r+1,c]]]
      if(!is.null(nn)) {
        if(checkNewNode(map, distance, nn[1], nn[2])) {
          newNodes = append(newNodes, list(nn))
        }
      }
    }
    if(checkNode(map, distance, r-1, c, c("|", "F","7"))) {
      distance[r-1, c] = i
      nn = list("|"=c(r-2, c), "7"=c(r-1,c-1), "F"=c(r-1,c+1))[[map[r-1,c]]]
      if(!is.null(nn)) {
        if(checkNewNode(map, distance, nn[1], nn[2])) {
          newNodes = append(newNodes, list(nn))
        }
      }
    }
    if(checkNode(map, distance, r, c+1, c("-", "7","J"))) {
      distance[r, c+1] = i
      nn = list("J"=c(r+1, c+1), "7"=c(r-1,c+1), "-"=c(r,c+2))[[map[r,c+1]]]
      if(!is.null(nn)) {
        if(checkNewNode(map, distance, nn[1], nn[2])) {
          newNodes = append(newNodes, list(nn))
        }
      }
    }
    if(checkNode(map, distance, r, c-1, c("-", "L","F"))) {
      distance[r, c-1] = i
      nn = list("F"=c(r+1, c-1), "L"=c(r-1,c-1), "-"=c(r,c-2))[[map[r, c-1]]]
      if(!is.null(nn)) {
        if(checkNewNode(map, distance, nn[1], nn[2])) {
          newNodes = append(newNodes, list(nn))
        }
      }
    }
  }
  print(distance)
  print(nodes)
  if(length(newNodes) == 0) {
    max(distance)
  } else {
    nextStep(map, distance, newNodes, i+1)  
  }
}


part1 = function(x) {
  map = t(sapply(x, function(y) stri_sub(y, 1:stri_length(y), len=1)))
  start = which(map=="S", TRUE)
  distance = matrix(-1, nrow(map), ncol(map))
  distance[start[1], start[2]] = 0
  nodes = list(start)
  nextStep(map, distance, nodes, 1)
}

part1(testInput) == 8
part1(input)

part2 = function(x) {
  sum(sapply(x, function(y) extrapolate(y, TRUE)))
}

part2(testInput) == 2
part2(input)
