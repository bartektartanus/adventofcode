
class Vector {
  int x;
  int y;
  int z;

  Vector(this.x, this.y, this.z);

  void add(Vector another) {
    x += another.x;
    y += another.y;
    z += another.z;
  }

  int absoluteSum(){
    return x.abs() + y.abs() + z.abs();
  }

  Vector compare(Vector another) {
    return Vector(-x.compareTo(another.x), -y.compareTo(another.y), -z.compareTo(another.z));
  }

  Vector copy(){
    return Vector(x,y,z);
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
          other is Vector &&
              runtimeType == other.runtimeType &&
              x == other.x &&
              y == other.y &&
              z == other.z;

  @override
  int get hashCode =>
      x.hashCode ^
      y.hashCode ^
      z.hashCode;

  @override
  String toString() {
    return 'Vector{x: $x, y: $y, z: $z}';
  }
}

class Moon {
  Vector position;
  Vector velocity = Vector(0, 0, 0);

  Moon(this.position);

  void move() {
    position.add(velocity);
  }

  void applyGravity(Moon another) {
    velocity.add(this.position.compare(another.position));
  }

  int energy(){
    return position.absoluteSum() * velocity.absoluteSum();
  }

  Moon copy(){
    var moon = Moon(position.copy());
    moon.velocity = velocity.copy();
    return moon;
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
          other is Moon &&
              runtimeType == other.runtimeType &&
              position == other.position &&
              velocity == other.velocity;

  @override
  int get hashCode =>
      position.hashCode ^
      velocity.hashCode;

  @override
  String toString() {
    return 'Moon{position: $position, velocity: $velocity}';
  }
}

class Space {
  Moon moon1;
  Moon moon2;
  Moon moon3;
  Moon moon4;

  Space(this.moon1, this.moon2, this.moon3, this.moon4);

  Space copy(){
    return Space(moon1.copy(), moon2.copy(), moon3.copy(), moon4.copy());
  }

  void move(){
    moon1.move();
    moon2.move();
    moon3.move();
    moon4.move();
  }

  int energy(){
    return moon1.energy() + moon2.energy() + moon3.energy() + moon4.energy();
  }

  Moon get(int i){
    switch(i){
      case 1:
        return moon1;
      case 2:
        return moon2;
      case 3:
        return moon3;
      case 4:
        return moon4;
    }
  }

  bool equalOnX(Space s){
    return
      moon1.position.x == s.moon1.position.x && moon1.velocity.x == s.moon1.velocity.x &&
      moon2.position.x == s.moon2.position.x && moon2.velocity.x == s.moon2.velocity.x &&
      moon3.position.x == s.moon3.position.x && moon3.velocity.x == s.moon3.velocity.x &&
      moon4.position.x == s.moon4.position.x && moon4.velocity.x == s.moon4.velocity.x;
  }


  bool equalOnY(Space s){
    return
      moon1.position.y == s.moon1.position.y && moon1.velocity.y == s.moon1.velocity.y &&
          moon2.position.y == s.moon2.position.y && moon2.velocity.y == s.moon2.velocity.y &&
          moon3.position.y == s.moon3.position.y && moon3.velocity.y == s.moon3.velocity.y &&
          moon4.position.y == s.moon4.position.y && moon4.velocity.y == s.moon4.velocity.y;
  }


  bool equalOnZ(Space s){
    return
      moon1.position.z == s.moon1.position.z && moon1.velocity.z == s.moon1.velocity.z &&
          moon2.position.z == s.moon2.position.z && moon2.velocity.z == s.moon2.velocity.z &&
          moon3.position.z == s.moon3.position.z && moon3.velocity.z == s.moon3.velocity.z &&
          moon4.position.z == s.moon4.position.z && moon4.velocity.z == s.moon4.velocity.z;
  }

  @override
  String toString() {
    return 'Space{moon1: $moon1, moon2: $moon2, moon3: $moon3, moon4: $moon4}';
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
          other is Space &&
              runtimeType == other.runtimeType &&
              moon1 == other.moon1 &&
              moon2 == other.moon2 &&
              moon3 == other.moon3 &&
              moon4 == other.moon4;

  @override
  int get hashCode =>
      moon1.hashCode ^
      moon2.hashCode ^
      moon3.hashCode ^
      moon4.hashCode;


}

//<x=-4, y=-14, z=8>
//<x=1, y=-8, z=10>
//<x=-15, y=2, z=1>
//<x=-17, y=-17, z=16>

//<x=-1, y=0, z=2>
//<x=2, y=-10, z=-7>
//<x=4, y=-8, z=8>
//<x=3, y=5, z=-1>
void main() {
  var test = Space(Moon(Vector(-1, 0, 2)), Moon(Vector(2, -10, -7)), Moon(Vector(4, -8, 8)), Moon(Vector(3, 5, -1)));
  var prod = Space(Moon(Vector(-4, -14, 8)), Moon(Vector(1, -8, 10)), Moon(Vector(-15, 2, 1)), Moon(Vector(-17, -17, 16)));
  var first = prod.copy();
  var space = first.copy();
  var steps = 1 << 32;
  var foundX = false;
  var foundY = false;
  var foundZ = false;
  int cycleX, cycleY, cycleZ;
  for(int step = 0; step < steps; step++){
    if(step % 1000000 == 0){
      print("step=$step");
    }
    for(int i = 1; i <= 4; i++){
      for(int j = 1; j <= 4; j++){
        if(i != j){
          space.get(i).applyGravity(space.get(j));
        }
      }
    }
    space.move();
    if(space.equalOnX(first) && !foundX){
      print("x step=$step");
      foundX = true;
      cycleX = step + 1;
    }
    if(space.equalOnY(first) && !foundY){
      print("y step=$step");
      cycleY = step + 1;
      foundY = true;
    }
    if(space.equalOnZ(first) && !foundZ){
      print("z step=$step");
      cycleZ = step + 1;
      foundZ = true;
    }
    if(foundX && foundY && foundZ){
      break;
    }
    if(space == first){
      print(step);
      print(space);
      break;
    }
  }
  var energy = space.energy();
  print("energy=$energy");
  print(gcd(cycleX, cycleY));
  print(lcm(cycleX, cycleY));
  print(gcd(cycleX * cycleY, cycleZ));
  print(lcm(cycleX * cycleY, cycleZ));
  print(lcm(cycleX, lcm(cycleY, cycleZ)));
}
// x 186027
// y 286331
// z 108343

int gcd(int a, int b){
  if(b == 0){
    return a;
  }else{
    return gcd(b, a%b);
  }
}

int lcm(int a, int b){
  return a*b~/gcd(a,b);
}