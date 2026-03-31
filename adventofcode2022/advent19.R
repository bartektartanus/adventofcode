require(stringi)

inputTest = c("Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.",
"Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")
input = readLines("~/workspace-private/adventofcode/adventofcode2022/input19.txt")


bestOre = function(cost, robot, ore, minute, r) {
  if(minute > 24) {
    return(ore[4])
  }
  if(r >= (ore[4] + (24-minute+1) * robot[4] + sum(seq_len(24-minute+1)))){
    return(r)
  }
  
  if(all(ore[c(1,3)] >= cost[5:6])) {
    newRobot = c(0,0,0,1)
    r = max(r, bestOre(cost, robot+newRobot, ore - c(cost[5], 0, cost[6], 0) + robot, minute + 1, r))
  } else {
    if(all(ore[c(1,2)] >= cost[3:4]) && robot[3] < cost[6]) {
      newRobot = c(0,0,1,0)
      r = max(r, bestOre(cost, robot+newRobot, ore - c(cost[3], cost[4], 0, 0) + robot, minute + 1, r))
    } 
    if(ore[1] >= cost[2] && robot[2] < cost[4]) {
      newRobot = c(0,1,0,0)
      r = max(r, bestOre(cost, robot+newRobot, ore - c(cost[2], 0, 0, 0) + robot, minute + 1, r))
    } 
    if(ore[1] >= cost[1] && robot[1] < max(cost[c(1,2,3,5)])) {
      newRobot = c(1,0,0,0)
      r = max(r, bestOre(cost, robot+newRobot, ore - c(cost[1], 0, 0, 0) + robot, minute + 1, r))
    }
    r = max(r, bestOre(cost, robot, ore + robot, minute + 1, r))
  }
  
  return(r)
  
}

part1 = function(input) {
  blueprint = matrix(as.integer(stri_match_first_regex(input, "Blueprint \\d+: Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.")[,-1]), nrow=length(input))  
  quality = 0
  for(row in 1:nrow(blueprint)) {
    robot = c(1,0,0,0)
    ore = c(0,0,0,0)
    cost = blueprint[row,]
    b = bestOre(cost, robot, ore, 1, 0)
    print(b)
    quality = quality + b*row
  }
  quality
}


part1(inputTest)
part1(input)
