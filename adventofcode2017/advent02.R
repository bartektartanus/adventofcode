input = read.table("~/workspace-private/adventofcode/adventofcode2017/input02.txt")

sum(apply(input, 1, function(x) diff(range(x))))

sum(apply(input, 1, function(x) for(a in sort(x)) {
  w = (x %% a == 0) & x != a
  if(any(w)) {
    return((x/a) [w])
  }
}))
