require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input02.txt")

forward <- sum(as.integer(stri_replace_first_fixed(input[stri_startswith_fixed(input, "forward ")], "forward ", "")))
depth <- sum(as.integer(stri_replace_first_fixed(stri_replace_first_fixed(input[!stri_startswith_fixed(input, "forward ")], "down ", ""), "up ", "-")))

forward * depth

aim <- 0
depth <- 0
horizontal <- 0
for(step in input) {
  if(stri_startswith_fixed(step, "forward ")) {
    h = as.integer(stri_replace_first_fixed(step, "forward ", ""))
    horizontal = horizontal + h
    depth = depth + aim * h
  } else if (stri_startswith_fixed(step, "down ")) {
    a = as.integer(stri_replace_first_fixed(step, "down ", ""))
    aim = aim + a
  } else {
    a = as.integer(stri_replace_first_fixed(step, "up ", ""))
    aim = aim - a
  }
}
forward * depth
