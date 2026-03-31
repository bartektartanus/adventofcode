
fun main() {
    fun part1(input: List<String>): Int {
        val register = Register()
        input.forEach {
            register.runInstruction(it)
        }
        return register.signal
    }

    fun part2(input: List<String>): String {
        val crt = CRT()
        input.forEach {
            crt.runInstruction(it)
        }
        return crt.image.joinToString(separator = "")
    }

    val testInput = readInput("input10test")
    check(part1(testInput) == 13140)

    val input = readInput("input10")
    part1(input).println()

    part2(testInput).println()
    part2(input).println()
}

class Register {
    var value = 1
    var cycle = 0
    var signal = 0
    fun runInstruction(i: String) {
        if(i.startsWith("addx")) {
            val v = i.split(" ")[1].toInt()
            nextCycle()
            nextCycle()
            value += v
        } else if(i.startsWith("noop")) {
            nextCycle()
        }
    }

    fun nextCycle() {
        cycle++
        if((cycle-20) % 40 == 0) {
            signal += cycle * value
        }
    }
}

class CRT {
    private var value = 1
    private var cycle = 0
    var image = mutableListOf<String>()
    fun runInstruction(i: String) {
        if(i.startsWith("addx")) {
            val v = i.split(" ")[1].toInt()
            nextCycle()
            nextCycle()
            value += v
        } else if(i.startsWith("noop")) {
            nextCycle()
        }
    }

    private fun nextCycle() {
        if(cycle % 40 >= value - 1 && cycle % 40 <= value + 1) {
            image.add("#")
        } else {
            image.add(".")
        }
        cycle++
        if(cycle % 40 == 0) {
            image.add("\n")
        }

    }
}