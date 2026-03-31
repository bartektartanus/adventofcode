fun main() {
    fun part1(input: List<String>): Int {
        return input.size
    }

    fun part2(input: List<String>): Int {
        return input.size
    }

    val testInput = readInput("input00test")
    check(part1(testInput) == 0)

    val input = readInput("input00")
    part1(input).println()

    check(part2(testInput) == 0)
    part2(input).println()
}
