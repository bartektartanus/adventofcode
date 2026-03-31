import java.lang.Exception

fun main() {
    fun part1(input: List<String>): Long {
        val monkeys = input.joinToString("\n").split("\n\n").map { parseMonkey(it) }
        val game = Game(monkeys)
        return game.result()
    }

    fun part2(input: List<String>): Long {
        val monkeys = input.joinToString("\n").split("\n\n").map { parseMonkey(it) }
        val r = monkeys.map { it.testDivisible }.reduce{a, i -> a * i}
        val game = Game(monkeys, 10000) { i -> i % r }
        return game.result()
    }

    val testInput = readInput("input11test")
    check(part1(testInput) == 10605L)

    val input = readInput("input11")
    part1(input).println()

    check(part2(testInput) == 2713310158L)
    part2(input).println()
}

class Game(private val monkeys: List<Monkey>, private val rounds: Int = 20, private val relief: (Long) -> Long = { i: Long -> i/3}) {
    fun result(): Long {
        repeat(rounds) {
            monkeys.forEach { monkey ->
                monkey.throwItems(relief).forEach {
                    monkeys[it.toMonkey].acceptItem(it.item)
                }
            }
        }
        return monkeys.map { it.inspectedItems }.sortedDescending().take(2).reduce { acc, i -> acc * i }
    }
}

fun parseMonkey(input: String): Monkey {
    val result = "Monkey (\\d+):\n\\s*Starting items: ([0-9, ]+)\n\\s*Operation: new = old (.*?)\n\\s*Test: divisible by (\\d+)\n\\s*If true: throw to monkey (\\d+)\n\\s*If false: throw to monkey (\\d+)".toRegex().find(input)
    val g = result!!.groupValues
    val operation = parseOperation(g[3])
    return Monkey(g[1].toLong(), g[2].split(", ").map { it.toLong() }, operation, g[4].toLong(), g[5].toInt(), g[6].toInt())
}

fun parseOperation(input: String): (item: Long) -> Long {
    val s = input.split(" ")
    val o = s[0]
    val x = s[1]
    val operation: (a: Long,b: Long) -> Long = when(o) {
        "*" -> {a,b -> a * b}
        "+" -> {a,b -> a + b}
        "-" -> {a,b -> a - b}
        "/" -> {a,b -> a / b}
        else -> throw Exception()
    }
    if(x == "old") {
        return {item -> operation(item, item)}
    } else {
        return {item -> operation(item, x.toLong())}
    }
}

class Monkey(private val number: Long, startingItems: List<Long>, private val operation: (old: Long) -> Long,
             val testDivisible: Long, private val throwIfTrue: Int, private val throwIfFalse: Int) {
    private var items = startingItems.toMutableList()
    var inspectedItems: Long = 0
        private set

    fun throwItems(relief: (Long) -> Long): List<ThrownItem> {
        val result = items.map {
            val worry = relief(operation(it))
            ThrownItem(worry, if(worry % testDivisible == 0L) throwIfTrue else throwIfFalse)
        }
        items = mutableListOf()
        inspectedItems += result.size
        return result
    }

    fun acceptItem(item: Long) {
        items.add(item)
    }
}

data class ThrownItem(val item: Long, val toMonkey: Int)