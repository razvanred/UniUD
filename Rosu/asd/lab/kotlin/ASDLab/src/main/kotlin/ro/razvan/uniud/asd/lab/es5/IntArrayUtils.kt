package ro.razvan.uniud.asd.lab.es5

import java.lang.Math.floorDiv
import java.util.regex.Pattern

private val INT_ARRAY_PATTERN = Pattern.compile("\\d+")

fun String.toIntArray(): IntArray {
    val matcher = INT_ARRAY_PATTERN.matcher(this)
    val list = arrayListOf<Int>()
    while (matcher.find()) {
        list.add(matcher.group().toInt())
    }
    return list.toIntArray()
}

fun IntArray.binarySearch(element: Int): Int = binarySearch(element, 0, size)

private tailrec fun IntArray.binarySearch(element: Int, startInclusive: Int, endExclusive: Int): Int {
    if (startInclusive >= endExclusive) {
        return -1
    }

    val x = floorDiv(startInclusive + endExclusive, 2)
    return when {
        this[x] == element -> x
        this[x] < element -> binarySearch(element, x + 1, endExclusive)
        else -> binarySearch(element, startInclusive, x)
    }
}