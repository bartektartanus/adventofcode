compare = function(left, right) {
  if (length(left) == 1 && length(right) == 1) {
    if (left != right) {
      return(left < right)
    }
    return(null)
  } else if (is.list(left) && is.list(right)) {
    for (i in 1:(min(length(left), length(right)))) {
      r = compare(left[i], right[i])
      if(is.logical(r)) {
        return(r)
      }
    }
    
    return(compare(length(left), length(right)))
  } else if (pair case (List<Object> left, int right)) {
    return compare(left, [right]);
  } else if (pair case (int left, List<Object> right)) {
    return compare([left], right);
  } else {
    throw ArgumentError("I do not know how to compare these! (${pair.$0.runtimeType}, ${pair.$1.runtimeType})");
  }
}

(Object parsed, int index)? _parse(String input, [int index = 0]) {
  RegExp comma = RegExp(r"\s*,\s*");
  RegExp number = RegExp(r"\d+");

  if (input[index] == "[") {
    int i = index + 1;

    List<Object> objects = [];
    while (i < input.length - 1 && input[i] != "]") {
      if (_parse(input, i) case (Object element, int index)) {
        objects.add(element);
        i = index;
      }

      if (comma.matchAsPrefix(input, i)?.group(0) case String separator) {
        i += separator.length;
      }
    }

    if (input[i] == "]") {
      return (objects, i + 1);
    }
  } else if (number.matchAsPrefix(input, index)?.group(0) case String span) {
    return (int.parse(span), index + span.length);
  }
}
Object? parse(String input) {
  if (_parse(input) case (Object value, _)) {
    return value;
  }
  return null;
}

void part1() {
  input = readLines("~/workspace-private/adventofcode/adventofcode2022/input13.txt")

  List<(Object, Object)> pairs = [];
  List<String> pair = [];
  for (String line in lines) {
    if (line.isNotEmpty) {
      pair.add(line);
    } else if ((parse(pair[0]), parse(pair[1])) case (Object left, Object right)) {
      pairs.add((left, right));
      pair.clear();
    }
  }

if ((parse(pair[0]), parse(pair[1])) case (Object left, Object right)) {
  pairs.add((left, right));
  pair.clear();
}

int sum = 0;
for (int i = 0; i < pairs.length; ++i) {
  if (pairs[i] case (Object left, Object right)) {
    /// This might look redundant, but it *is* nullable
    if (compare(left, right) case true) {
      sum += i + 1;
    }
  }
}

print(sum);
}

void part2() {
  const int left = 2;
  const int right = 6;
  
  List<String> lines = File("bin/day_13/assets/main.txt").readAsLinesSync();
  
  List<Object> packets = [left, right];
  for (String line in lines) {
    if (line.isEmpty) {
      continue;
    }
    
    if (_parse(line) case (Object parsed, _)) {
      packets.add(parsed);
    }
  }
  
  packets.sort((left, right) => compare(left, right) ?? true ? -1 : 1);
  int decoderKey = (packets.indexOf(left) + 1) * (packets.indexOf(right) + 1);
  
  print(decoderKey);
}

void main() {
  part1();
  part2();
}
