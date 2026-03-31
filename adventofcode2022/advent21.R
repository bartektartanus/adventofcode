require(stringi)

input = readLines("~/workspace-private/adventofcode/adventofcode2022/input21.txt")
inputTest = stri_split_fixed("root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32", "\n")[[1]]

parse = function(v, values) {
  i = as.complex(v)
  if(is.na(i)) {
    return(values[v])
  } else {
    return(i)
  }
} 

solve = function(input) {
  values = rep(NA_complex_, length(input))
  names(values) = stri_match_first_regex(input, "(\\w+):")[,-1]
  
  while(any(is.na(values))) {
    for(line in input[is.na(values)]) {
      name = stri_match_first_regex(line, "(\\w+):")[,-1]
      v = stri_match_first_regex(line, ": ([0-9i+]+)$")[,-1]
      if(!is.na(v)) {
        values[name] = as.complex(v)
      } else {
        x = stri_match_first_regex(line, ": (.+?) ([+\\-*/]) (.+?)$")[,-1]
        a = parse(x[1], values)
        b = parse(x[3], values)
        r = switch(x[2],
                   "+" = a+b,
                   "-" = a-b,
                   "*" = a*b,
                   "/" = a/b
        )
        if(!is.na(r)) {
          values[name] = r
        }
      }
    }
  }
  return(values)
}

part1 = function(input) {
  Re(solve(input)["root"])
}
part1(inputTest)
print(part1(input), digits=22)

part2 = function(input) {
  input[stri_startswith_fixed(input, "humn")] = "humn: 0+1i"
  values = solve(input)
  x = stri_match_first_regex(input[stri_startswith_fixed(input, "root:")], ": (.+?) ([+\\-*/]) (.+?)$")[,-1]
  a = values[x[1]]
  b = values[x[3]]
  Re(a-b)/(Im(b-a))
}
part2(inputTest)
print(part2(input), digits=22)
