fun main() {
    fun part1(input: List<String>): Int {
        val valves = input.map {
            val match = "Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)".toRegex().find(it)!!
            Valve(match.groupValues[1], match.groupValues[2].toInt(),
                match.groupValues[3].split(", ").associateWith { 1 })
        }.associateBy { it.name }
        val network = Network(valves)
        val result = network.solve()
        return result.pressure
    }

    fun part2(input: List<String>): Int {
        return input.size
    }

    val testInput = readInput("input16test")
    val testResultPart1 = part1(testInput)
    testResultPart1.println()
    check(testResultPart1 == 1651)

    val input = readInput("input16")
    part1(input).println()

    check(part2(testInput) == 0)
    part2(input).println()
}

class Network(private val valves: Map<String, Valve>) {
    private var current: String = "AA"
    var openedValves: MutableSet<Valve> = mutableSetOf()
    private val nonZeroRateValves: Set<String> = valves.values.filter { it.rate > 0 }.map { it.name }.toSet()
    private val reducedValves = reduceValves()
    private var time = 0
    var pressure = 0

    fun reduceValves(): Map<String, Valve> {
        return valves.values.filter { it.rate > 0 || it.name == "AA" }.map { reduceValve(it) }.associateBy { it.name }
    }

    private fun reduceValve(valve: Valve): Valve {
        val distance = mutableMapOf<String, Int>()
        var d = 0
        var n = valve.neighbours.keys
        while (!distance.keys.toSet().plus(valve.name).containsAll(nonZeroRateValves)) {
            d += 1
            n.filter { nonZeroRateValves.contains(it) }.filter { it != valve.name }.forEach {
                distance.putIfAbsent(it, d)
            }
            n = n.flatMap { valves[it]!!.neighbours.keys.toList() }.toSet()

        }
        return Valve(valve.name, valve.rate, distance)
    }

    fun solve(): Network {
        if (time >= 30) {
            return this
        }
        val c = reducedValves[current]!!
        if (c.rate > 0 && !openedValves.contains(c)) {
            advanceTime(1)
            openedValves.add(c)
        }
        val n = c.neighbours.filter { !openedValves.map { it.name }.contains(it.key) }
        return if (n.isEmpty()) {
            advanceTime(30 - time)
            this
        } else {
            n.map {
                val newNetwork = copyWithCurrent(it.key)
                val truncatedTime = minOf(30-time, it.value)
                newNetwork.advanceTime(truncatedTime)
                newNetwork.solve()
            }.maxBy { it.pressure }
        }
    }

    private fun advanceTime(times: Int) {
        time += times
        pressure += (openedValves.sumOf { it.rate }) * times
    }

    private fun copyWithCurrent(newCurrent: String): Network {
        val copy = Network(valves)
        copy.let {
            it.current = newCurrent
            it.time = time
            it.openedValves = openedValves.toMutableSet()
            it.pressure = pressure
        }
        return copy
    }
}

class Valve(val name: String, val rate: Int, val neighbours: Map<String, Int>) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Valve

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    override fun toString(): String {
        return "Valve(name='$name', rate=$rate, neighbours=$neighbours)"
    }

}