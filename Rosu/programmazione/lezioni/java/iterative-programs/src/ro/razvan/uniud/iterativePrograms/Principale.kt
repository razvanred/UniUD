package ro.razvan.uniud.iterativePrograms

fun gcdRec(x: Int, y: Int): Int = when {
    x == y -> x
    x < y -> gcdRec(x = x, y = y - x)
    else -> gcdRec(x = x - y, y = y)
}

fun gcdIter(x: Int, y: Int): Int {
    var u = x
    var v = y

    while (u != v) {

        if (u > v) {
            u -= v
        } else {
            v -= u
        }

    }

    return u
}

fun lcmRec(x: Int, y: Int): Int {
    return lcmTailRec(x, y, x)
}

fun lcmTailRec(x: Int, y: Int, s: Int): Int = if (s % y == 0) {
    s
} else {
    lcmTailRec(x, y, s + x)
}

fun lcmIter(x: Int, y: Int): Int {
    var lcm = x

    while (lcm % y != 0) {
        lcm += x
    }

    return lcm
}

fun Int.isEven() = this % 2 == 0

fun isPrimeRec(n: Int): Boolean {

    return if (n.isEven()) {
        n == 2
    } else {
        !hasOddDivisorsInRangeRec(n, 3, Math.floor(Math.sqrt(n.toDouble())).toInt())
    }

}

fun isPrimeIter(n: Int): Boolean {

    if (n.isEven()) {

        return n == 2

    } else {

        var i = 3

        while (i < Math.floor(Math.sqrt(n.toDouble()))) {
            if (n % i == 0) {
                return false
            }
            i += 2
        }

        return true
    }
}

fun hasOddDivisorsInRangeRec(n: Int, start: Int, end: Int): Boolean = when {
    start > end -> false
    n % start == 0 -> true
    else -> hasOddDivisorsInRangeRec(n, start + 2, end)
}

fun ufoRec(x: Int): Int = when {
    x == 1 -> 1
    x.isEven() -> ufoRec(x / 2) * 2 - 1
    else -> ufoRec(x / 2) * 2 + 1
}

fun ufoIter(x: Int): Int {
    val stack = IntArray(32)
    var t = 0
    var n = x

    while (n > 1) {
        stack[t] = n
        n /= 2
        t++
    }

    var v = 1

    while (--t >= 0) {

        v = v * 2 + if (stack[t].isEven()) {
           - 1
        } else {
            1
        }

    }

    return v
}

fun main() {
    println(isPrimeIter(23))
    println(!isPrimeIter(45))
    println(ufoIter(12) == ufoRec(12))
}