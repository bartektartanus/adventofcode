require(stringi)
input = readLines("~/workspace-private/adventofcode/adventofcode2022/input07.txt")
input = stri_split_fixed(c("$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"), "\n")[[1]]

dirStack = "/"
dirSize = c("/" = 0)
lsDir = c()
for(line in input) {
  if(line == "$ cd .."){
    dirStack = head(dirStack, -1)
  } else if(stri_startswith_fixed(line, "$ cd")) {
    d = stri_match_first_regex(line, "^\\$ cd (.+)$")[,2]
    dirStack = c(dirStack, d)
    currentDir = paste0(dirStack, collapse = " ")
    if(is.na(dirSize[currentDir])) {
      dirSize[currentDir] = 0
    }
  } else if(stri_detect_regex(line, "^[0-9]+")) {
    s = as.integer(stri_match_first_regex(line, "^[0-9]+")[,1])
    for(i in seq_along(dirStack)) {
      dirSize[paste0(dirStack[1:i], collapse = " ")] = dirSize[paste0(dirStack[1:i], collapse = " ")] + s
    }
  }
}

sum(dirSize[dirSize < 100000])

# part 2

min(dirSize[dirSize > (30000000 - (70000000 - dirSize["/"]))])

