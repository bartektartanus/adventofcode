# [N]             [R]             [C]
# [T] [J]         [S] [J]         [N]
# [B] [Z]     [H] [M] [Z]         [D]
# [S] [P]     [G] [L] [H] [Z]     [T]
# [Q] [D]     [F] [D] [V] [L] [S] [M]
# [H] [F] [V] [J] [C] [W] [P] [W] [L]
# [G] [S] [H] [Z] [Z] [T] [F] [V] [H]
# [R] [H] [Z] [M] [T] [M] [T] [Q] [W]
#  1   2   3   4   5   6   7   8   9 
require(stringi)
input = lapply(list(c("R G H Q S B T N"), c("H S F D P Z J"), c("Z H V"), c("M Z J F G H"), 
            "T Z C D L M S R", "M T W V H Z J", "T F P L Z", "Q V W S", "W H L M T D N C"), FUN=function(x) stri_split_fixed(x, " ")[[1]])

moves = readLines("~/workspace-private/adventofcode/adventofcode2022/input05.txt")

current = input
for(move in moves) {
  x = as.integer(stri_match_first_regex(move, "move (\\d+) from (\\d) to (\\d)")[,-1])
  count = x[1]
  from = x[2]
  to = x[3]
  for(i in seq_len(count)) {
    current[[to]] = c(current[[to]], tail(current[[from]],1))
    current[[from]] = head(current[[from]], -1)
  }
}
current

stri_paste(lapply(current, FUN=function(x) tail(x, 1)), collapse="")

# part 2

current = input
for(move in moves) {
  x = as.integer(stri_match_first_regex(move, "move (\\d+) from (\\d) to (\\d)")[,-1])
  count = x[1]
  from = x[2]
  to = x[3]
  current[[to]] = c(current[[to]], tail(current[[from]],count))
  current[[from]] = head(current[[from]], -count)
}
current

stri_paste(lapply(current, FUN=function(x) tail(x, 1)), collapse="")
