import kotlin.math.absoluteValue
import kotlin.math.sign

fun main() {
    fun rope(input: List<String>, knots: Int): Int {
        val rope = Rope(knots)
        input.forEach {
            rope.move(Move(it))
        }
        return rope.visitedByTail.size
    }

    fun part1(input: List<String>): Int {
        return rope(input, 1)
    }

    fun part2(input: List<String>): Int {
        return rope(input, 9)
    }

    val testInput = readInput("input09test")
    check(part1(testInput) == 13)

    val input = readInput("input09")
    part1(input).println()
    part2(input).println()
}

data class Point(val x: Int, val y: Int) {
    fun add(point: Point): Point {
        return Point(this.x + point.x, this.y + point.y)
    }
}
class Move(raw: String) {
    val direction: String
    val length: Int;
    init {
        val s = raw.split(" ")
        direction = s[0]
        length = s[1].toInt()
    }
    fun singleMove(): Point {
        return when(direction) {
            "R" -> Point(1,0)
            "L" -> Point(-1,0)
            "U" -> Point(0,1)
            "D" -> Point(0,-1)
            else -> Point(0, 0)
        }
    }
}
class Rope(knotsCount: Int = 1) {
    private var head = Point(0,0)
    private var knots = (1..knotsCount).map { head }
    val visitedByTail = mutableSetOf(head)

    fun move(move: Move) {
        repeat(move.length) {
            head = head.add(move.singleMove())
            var prevKnot = head
            knots = knots.map {
                val next = moveKnot(prevKnot, it)
                prevKnot = next
                next
            }
            visitedByTail.add(knots.last())
        }
    }

    private fun moveKnot(prev: Point, current: Point): Point {
        val xDist = (prev.x - current.x).absoluteValue
        val yDist = (prev.y - current.y).absoluteValue
        val x = (prev.x - current.x).sign
        val y = (prev.y - current.y).sign
        val point = if((xDist >= 1 && yDist > 1) || (xDist > 1 && yDist >= 1)) {
            Point(x,y)
        } else if(xDist > 1) {
            Point(x, 0)
        } else if(yDist > 1) {
            Point(0, y)
        } else {
            Point(0,0)
        }
        return current.add(point)
    }
}