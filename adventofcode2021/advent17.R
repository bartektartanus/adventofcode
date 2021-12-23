require(stringi)
input <- readLines("~/workspace-private/adventofcode/adventofcode2021/input17.txt")

input = "target area: x=25..67, y=-260..-200"
tx = 25:67
ty = -200:-260

run = function(v, pos=c(0,0), h=0, x1=25, x2=67, y1=-260, y2=-200) {
  #cat("\nv=",v," p=",pos," h=",h)
  if(pos[1] >=x1 && pos[1] <= x2 && pos[2] >= y1 && pos[2] <= y2){
    list(result=TRUE, height=h)
  } else if(pos[1] <= x2 && pos[2] >= y1) {
    newV = v - 1
    newV[1] = max(newV[1],0)
    newPos = pos + v
    newH = max(h, newPos[2])
    run(newV, newPos, newH, x1, x2, y1, y2)
  } else {
    list(result=FALSE)
  }
}

run(c(9,0), c(0,0), 0, 20,30,-10,-5)
run(c(6,3), c(0,0), 0, 20,30,-10,-5)
run(c(6,9), c(0,0), 0, 20,30,-10,-5)
run(c(1,1), c(0,0), 0, 20,30,-10,-5)

maxH = 0
count = 0
for(y in -260:10000){
  for(x in 1:67) {
    r = run(c(x,y))
    if(r$result) {
      count = count+1
      if(r$height > maxH) {
        maxH = r$height
        cat("\nh=",maxH, " c=", count)
      }
    }
  }
  cat("\nh=",maxH, " c=", count, " y=",y)
}
