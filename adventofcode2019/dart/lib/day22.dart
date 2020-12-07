
import 'dart:io';
import 'dart:math';

void main(){
  var lines = new File("res/input22.txt").readAsLinesSync();
  var deck = List.generate(10007, (i) => i);

  int position = 2019;
  lines.forEach((line){
    deck = shuffle(deck, line);
    position = shufflePosition(position, 10007, line);
    print("deck=${deck.indexOf(2019)} pos=$position line=$line");
  });
  print(deck.indexOf(2019)); // 1879
  print(position);


  position = 2020;
  var start = DateTime.now();
  for(int i = 1; i <= 101741582076661; i++){
    lines.forEach((line){
      position = shufflePosition(position, 119315717514047, line);
    });
    if(i % 100000 == 0){
      var progress = i.toDouble() / 101741582076661;
      var d = Duration(seconds: DateTime.now().difference(start).inSeconds ~/ progress);
      print("progess=${progress} time=$d}");
    }
  }
  print(position);

}

List<int> shuffle(List<int> deck, String line){
  if(line == "deal into new stack"){
    return deck.reversed.toList();
  }else if(line.startsWith("cut")){
    int cut = int.parse(line.replaceFirst("cut ", ""));
    if(cut < 0){
      cut = deck.length + cut;
    }
    List<int> result = List(deck.length);
    List.copyRange(result, 0, deck, cut);
//    print("deck=${deck.length} cut=$cut");
    List.copyRange(result, deck.length - cut, deck, 0, cut);

    return result;
  }else if(line.startsWith("deal with increment ")){
    int increment = int.parse(line.replaceFirst("deal with increment ", ""));
    List<int> result = List(deck.length);
    int position = 0;
    for(int i in deck){
      result[position] = i;
      position += increment;
      position %= deck.length;
    }
    return result;
  }
}

int shufflePosition(int position, int deckSize, String line){
  if(line == "deal into new stack"){
    return deckSize - position - 1;
  }else if(line.startsWith("cut")){
    int cut = int.parse(line.replaceFirst("cut ", ""));
    if(cut < 0){
      cut = deckSize + cut ;
    }
    if(position < cut){
      return position + deckSize - cut;
    }else{
      return position - cut ;
    }
  }else if(line.startsWith("deal with increment ")){
    int increment = int.parse(line.replaceFirst("deal with increment ", ""));
    return position * increment % deckSize;
  }
}

