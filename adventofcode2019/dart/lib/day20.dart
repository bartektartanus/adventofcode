import 'dart:io';
import 'dart:math';

import 'package:charcode/ascii.dart';


void main(){
  var rows = new File("res/input20.txt").readAsLinesSync().map((l) => l.codeUnits);
  var array = rows.reduce((a,b){return List.of(a)..addAll(b);});
  var map = LabyrinthMap(array, rows.length, rows.first.length);
  map.printMap();
  map.simplify();
  map.printMap();
  var path = map.findPath();
  print(path);
  print("cacheLength=${LabyrinthMap.cache.length} count=${LabyrinthMap.cacheCount}");
}

class Solver {

}

class Cell {
  int row;
  int col;

  Cell(this.row, this.col);

  factory Cell.fromIndex(int i, int cols){
    int row = i % cols;
    int col = i ~/ cols;
    return Cell(row, col);
  }

  Cell nextCol() => Cell(row, col + 1);
  Cell prevCol() => Cell(row, col - 1);
  Cell nextRow() => Cell(row + 1, col);
  Cell prevRow() => Cell(row - 1, col);

  int index(int cols){
    return row * cols + col;
  }

  List<Cell> neighbours(){
    return [nextCol(), prevCol(), nextRow(), prevRow()];
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
          other is Cell && runtimeType == other.runtimeType && row == other.row && col == other.col;

  @override
  int get hashCode => row.hashCode ^ col.hashCode;

  @override
  String toString() {
    return 'Cell{row: $row, col: $col}';
  }
}

class LabyrinthMap{

  static const int EMPTY = $dot;
  static const int WALL = $hash;
  static const int PERSON = $at;
  static const int inf = 2 << 31;

  static final Map<String, int> cache = Map();
  static int cacheCount = 0;
  static int skipped = 0;

  List<int> a;
  List<int> remainingKeys;
  int rows;
  int cols;


  LabyrinthMap(this.a, this.rows, this.cols);

  int get(cell){
    var index = cell.index(cols);
    return index < rows * cols ? a[index] : $hash;
  }

  void set(int row, int col, int value){
    a[row * cols + col] = value;
  }

  String getCell(Cell cell){
    return String.fromCharCode(get(cell));
  }

  bool isEmpty(int row, int col) => get(Cell(row, col)) == EMPTY;
  bool isCellEmpty(Cell cell) => get(cell) == EMPTY;

  bool isWall(int row, int col) => get(Cell(row, col)) == WALL;

  bool isUpper(Cell cell) {
    var c = get(cell);
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
        for (int j = 1; j < cols - 1; j++) {
          if(isEmpty(i, j) && countWallsAround(i,j) == 3){
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

  bool isAnyPortal(Cell cell){
    return cell.col + 1 < cols && isUpper(cell) && isUpper(cell.nextCol());
  }

  bool isPortal(int i, int j, String portal){
    return j+1 < cols && getPortal(Cell(i, j)) == portal;
  }

  String getPortal(Cell cell){
    return (getCell(cell) + getCell(cell.nextCol()));
  }

  Cell findMatchingPortal(Cell cell){
    var portal = getPortal(cell);
    for(int i = 0; i < rows; i++){
      for(int j = 0; j < cols; j++){
        if(isPortal(i, j, portal) && cell != Cell(i,j)){
          return Cell(i,j);
        }
      }
    }
    print("NOT FOUND MATCHING FOR $portal");
    return null;
  }

  bool allKeysCollected(){
    return a.firstWhere((i) => isKey(i), orElse: () => $dot) == $dot;
  }

  LabyrinthMap copy(){
    return new LabyrinthMap(List.of(a), rows, cols);
  }


  bool isCellEmptyOrKey(Cell cell) {
    return isKey(get(cell)) || get(cell) == EMPTY;
  }

  bool isEmptyOrKey(int i) {
    return isKey(i) || i == EMPTY;
  }

  Map<int, int> distanceToKey(){
    List<int> distance = List.filled(rows*cols, inf);
    int startIndex = 0;
    int indexOf = a.indexOf(PERSON, startIndex);
    while(indexOf != -1){
      int startingRow = indexOf ~/ cols;
      int startingCol = indexOf % cols;
      Cell starting = Cell(startingRow, startingCol);
      distance[starting.index(cols)] = 0;
      for(var c in starting.neighbours()){
        if(isCellEmptyOrKey(c)){
          populateDistance(distance, starting, c);
        }
      }
      startIndex = indexOf + 1;
      indexOf = a.indexOf(PERSON, startIndex);
    }

    String line = "";
    for(int i =0; i < distance.length; i++){
      if(i % cols == 0){
        print(line);
        line = "";
      }
      if(distance[i] < 2 << 31 ){
        line += distance[i].toString().padLeft(4,' ');
      }else {
        line += '  ${String.fromCharCode(a[i])} ';
      }
    }
    print(line);

    Map<int, int> result = Map();
    for(int i = 0; i < rows * cols; i++){
      if(isKey(a[i]) && distance[i] < inf){
        result[a[i]] = distance[i];
      }
    }
    return result;
  }


  void populateDistance(List<int> currentDistance, Cell prevCell, Cell cell){ //int p,int d, int od){
    var cellIndex = cell.index(cols);
    currentDistance[cellIndex] = min(currentDistance[cellIndex], currentDistance[prevCell.index(cols)] + 1);
    var cd = currentDistance[cellIndex];
    for(var c in cell.neighbours()){
      if(currentDistance[c.index(cols)] > cd + 1 && isCellEmptyOrKey(c) && prevCell != c){
        populateDistance(currentDistance, cell, c);
      }
      if(isAnyPortal(c) && getPortal(c) != "ZZ"){
        var matching = findMatchingPortal(c);
        currentDistance[matching.index(cols)] = cd + 1;
        var nextCell = matching.nextCol().nextCol();
        if(!isCellEmpty(nextCell)){
          nextCell = matching.prevCol();
          if(!isCellEmpty(nextCell)){
            nextCell = matching.nextRow();
            if(!isCellEmpty(nextCell)){
              nextCell = matching.prevRow();
            }
          }
        }
        if(nextCell != prevCell){
          populateDistance(currentDistance, cell, nextCell);
        }
      }
    }
    if(isAnyPortal(cell.prevCol().prevCol())){
      var matching = findMatchingPortal(cell.prevCol().prevCol());
      currentDistance[matching.index(cols)] = cd + 1;
      var nextCell = matching.nextCol().nextCol();
      if(!isCellEmpty(nextCell)){
        nextCell = matching.prevCol();
        if(!isCellEmpty(nextCell)){
          nextCell = matching.nextRow();
          if(!isCellEmpty(nextCell)){
            nextCell = matching.prevRow();
          }
        }
      }
      if(nextCell != prevCell){
        populateDistance(currentDistance, cell, nextCell);
      }
    }
//    p = p+d;
//    currentDistance[p] = min(currentDistance[p], currentDistance[p - d] + 1);
//    var cd = currentDistance[p];
//    if(currentDistance[p + d] > cd+1 && isEmptyOrKey(a[p+d])){
//      populateDistance(currentDistance, p, d, od);
//    }
//    if(currentDistance[p + od] > cd+1 && isEmptyOrKey(a[p+od])){
//      populateDistance(currentDistance, p, od, d);
//    }
//    if(currentDistance[p - od] > cd+1 && isEmptyOrKey(a[p-od])){
//      populateDistance(currentDistance, p , -od, d);
//    }
//    // portals
//    if(currentDistance[p + d] > cd+1 && isAnyPortal(Cell.fromIndex(p+d, rows))){
//      populateDistance(currentDistance, p, d, od);
//    }
//    if(currentDistance[p + od] > cd+1 && isAnyPortal(Cell.fromIndex(p+od,rows))){
//      populateDistance(currentDistance, p, od, d);
//    }
//    if(currentDistance[p - od] > cd+1 && isAnyPortal(Cell.fromIndex(p-od,rows))){
//
//      populateDistance(currentDistance, p , -od, d);
//    }
  }

  void moveToKey(int key) {
    a[a.indexOf(PERSON)] = EMPTY;
    a[a.indexOf(key)] = PERSON;
    int indexOfDoor = a.indexOf(String.fromCharCode(key).toUpperCase().codeUnits.first);
    if(indexOfDoor > -1) {
      a[indexOfDoor] = EMPTY;
    }
  }

  Path findPath(){
    if(allKeysCollected()){
      return Path([], 0);
    }else{
      var distance = distanceToKey();
      List<MapEntry<int, int>> sortedDistance = distance.entries.toList()..sort((a,b) => a.value.compareTo(b.value));

//      print(sortedDistance);
      Path shortest = Path([], inf);
      for(var e in sortedDistance){
        int key = e.key;
        var nextMap = this.copy();
        nextMap.moveToKey(key);
        var restOfThePath = nextMap.findPath();
        Path nextPath = Path([], e.value + restOfThePath.length);
        if(nextPath.length <= shortest.length){
          shortest = nextPath;
//          nextMap.printMap();
        }
      }
      return shortest;

    }

  }

  void printMap(){
    print("len=${a.length} r=$rows c=$cols");
    String line = "";
    for(int i =0; i < a.length; i++){
      if(i % cols == 0){
        print(line);
        line = "";
      }
      line += String.fromCharCode(a[i]);
    }
    print(line);
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
    return 'Path{path: ${path.map((p) => String.fromCharCode(p)).fold("",(a,b) => a+b)}, length: ${length == 2 << 31 ? 'inf' : length}}';
  }


}
