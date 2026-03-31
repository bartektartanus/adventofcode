import java.io.File
import java.math.BigInteger
import java.security.MessageDigest

object Utils

fun readInput(name: String) = File(Utils.javaClass.getResource("$name.txt").file).readLines()

fun String.md5() = BigInteger(1, MessageDigest.getInstance("MD5").digest(toByteArray()))
        .toString(16)
        .padStart(32, '0')

fun Any?.println() = println(this)
