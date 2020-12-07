import 'dart:io';
import 'dart:math';

import 'package:charcode/ascii.dart';

class Day18 {

  LabyrinthMap map;

}

void main(){
  var rows = new File("res/input18.txt").readAsLinesSync().map((l) => l.codeUnits);
  var map = LabyrinthMap(rows.reduce((a,b){return List.of(a)..addAll(b);}), rows.length);
  map.printMap();
  map.simplify();
  map.printMap();
  map.findPath(Path([],0));
}

class Solver {

}

class LabyrinthMap{

  static const int EMPTY = $dot;
  static const int WALL = $hash;
  static const int PERSON = $at;
  static const int inf = 2 << 31;

  static final Map<String, Map<int, int>> cache = Map();
  static int cacheCount = 0;
  static int skipped = 0;
  static Path shortestPath = Path([], 2 << 31);

  List<int> a;
  int rows;


  LabyrinthMap(this.a, this.rows);

  int get(int row, int col){
    return a[row + col * rows];
  }

  void set(int row, int col, int value){
    a[row + col*rows] = value;
  }

  String getCell(int row, int col){
    return String.fromCharCode(get(row,col));
  }

  bool isEmpty(int row, int col) => get(row, col) == EMPTY;

  bool isWall(int row, int col) => get(row, col) == WALL;

  bool isDoor(int row, int col) {
    var c = get(row, col);
    return c >= $A && c <= $Z;
  }

  bool isKey(int v) {
    return v >= $a && v <= $z;
  }

  void simplify(){
    bool anythingChanged = true;
    while(anythingChanged){
      anythingChanged = false;
      for(int i = 1; i < rows - 1; i++) {
        for (int j = 1; j < rows - 1; j++) {
          if((isEmpty(i, j) || isDoor(i, j)) && countWallsAround(i,j) == 3){
            anythingChanged = true;
            set(i,j,WALL);
          }
        }
      }
    }
  }

  int countWallsAround(int row, int col){
    int count = 0;
    if(isWall(row + 1, col)){
      count++;
    }
    if(isWall(row - 1, col)){
      count++;
    }
    if(isWall(row, col + 1)){
      count++;
    }
    if(isWall(row, col - 1)){
      count++;
    }
    return count;
  }

  bool allKeysCollected(){
    return a.firstWhere((i) => isKey(i), orElse: () => $dot) == $dot;
  }

  LabyrinthMap copy(){
    return new LabyrinthMap(List.of(a), rows);
  }

  bool isEmptyOrKey(int i) {
    return isKey(i) || i == EMPTY;
  }

  Map<int, int> distanceToKey(){
    int indexOf = a.indexOf(PERSON);
    int startingCol = indexOf ~/ rows;
    int startingRow = indexOf % rows;
    String keys = (a.where((i) => isKey(i)).toList()..sort()).map((i) => String.fromCharCode(i)).reduce((a,b) => a+b);
    if(keys.isEmpty){
      print("EMPTY");
      printMap();
    }
    String cacheKey = "$indexOf$keys";
    if(cache.containsKey(cacheKey)){
      cacheCount++;
      if(cacheCount % 10000 == 0){
        print("cache hit, size=${cache.length} count=$cacheCount keyLen=${keys.length} $cacheKey");
      }
      return cache[cacheKey];
    }
    List<int> distance = List.filled(rows*rows, inf);
    distance[startingRow + startingCol * rows] = 0;
    if(isEmptyOrKey(get(startingRow, startingCol+1))){
      populateDistance(distance, indexOf, rows, 1);
    }
    if(isEmptyOrKey(get(startingRow, startingCol-1))) {
      populateDistance(distance, indexOf, -rows, 1);
    }
    if(isEmptyOrKey(get(startingRow+1, startingCol))) {
      populateDistance(distance, indexOf, 1, rows);
    }
    if(isEmptyOrKey(get(startingRow-1, startingCol))) {
      populateDistance(distance, indexOf, -1, rows);
    }
    Map<int, int> result = Map();
    for(int i = 0; i < rows * rows; i++){
      if(isKey(a[i]) && distance[i] < inf){
        result[a[i]] = distance[i];
      }
    }
    cache[cacheKey] = result;
    return result;
  }


  void populateDistance(List<int> currentDistance, int p,int d, int od){
    p = p+d;
    currentDistance[p] = min(currentDistance[p], currentDistance[p - d] + 1);
    var cd = currentDistance[p];
    if(currentDistance[p + d] > cd+1 && isEmptyOrKey(a[p+d])){
      populateDistance(currentDistance, p, d, od);
    }
    if(currentDistance[p + od] > cd+1 && isEmptyOrKey(a[p+od])){
      populateDistance(currentDistance, p, od, d);
    }
    if(currentDistance[p - od] > cd+1 && isEmptyOrKey(a[p-od])){
      populateDistance(currentDistance, p , -od, d);
    }
  }

  void moveToKey(int key) {
    a[a.indexOf(PERSON)] = EMPTY;
    if(a.indexOf(key) == -1){
      printMap();
    }
    a[a.indexOf(key)] = PERSON;
    int indexOfDoor = a.indexOf(String.fromCharCode(key).toUpperCase().codeUnits.first);
    if(indexOfDoor > -1) {
      a[indexOfDoor] = EMPTY;
    }
  }

  void findPath(Path path){
    if(allKeysCollected()){
      if(shortestPath.length > path.length){
        shortestPath = path;
        print("new shortest path = $path");
      }
    }else{
      var distance = distanceToKey();
      List<MapEntry<int, int>> sortedDistance = distance.entries.toList()..sort((a,b) => a.value.compareTo(b.value));

//      print(sortedDistance);
      for(var e in sortedDistance){
        int key = e.key;
        var nextMap = this.copy();
        nextMap.moveToKey(key);
        var nextPath = path.add(key, e.value);
        if(nextPath.length <= shortestPath.length){
          nextMap.findPath(nextPath);
        }else{
          skipped++;
          if(skipped % 100000 == 0){
            print("skipped=$skipped current=${path.path} dropDepth=${distance.length-1},setdiff(names(distance),key),nextLen=${nextPath.length} global=globalShortestPath");
          }
        }

      }
    }

  }

  void printMap(){
    String line = "";
    for(int i =0; i < a.length; i++){
      if(i % rows == 0){
        print(line);
        line = "";
      }
      line += String.fromCharCode(a[i]);
    }
    print(line);
  }

  List<Key> findKeys(){
    return [];
  }
}

class Path {
  final List<int> path;
  final int length;

  Path(this.path, this.length);

  Path add(int key, int length){
    return new Path(List.of(path)..add(key), this.length + length);
  }

  @override
  String toString() {
    return 'Path{path: ${path.map((p) => String.fromCharCode(p)).reduce((a,b) => a+b)}, length: $length}';
  }


}

class Key {
  int distanceFromCenter;
  List<int> blockedByDoors;
  int quarter;
}