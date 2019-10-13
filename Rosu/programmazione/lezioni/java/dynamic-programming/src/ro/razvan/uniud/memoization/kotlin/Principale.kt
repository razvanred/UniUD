package ro.razvan.uniud.memoization.kotlin

import ro.razvan.uniud.memoization.kotlin.LongestCommonSubsequence.lcs
import ro.razvan.uniud.memoization.kotlin.LongestCommonSubsequence.llcsIter
import ro.razvan.uniud.memoization.kotlin.LongestCommonSubsequence.llcsMemRec
import ro.razvan.uniud.memoization.kotlin.LongestCommonSubsequence.llcsRec

private const val UNKNOWN = 0

fun fibonacciRec(n: Int): Int {
    return if (n < 2) {
        1
    } else {
        fibonacciRec(n - 2) + fibonacciRec(n - 1)
    }
}

fun fibonacciMem(
    n: Int, array: IntArray = IntArray(n + 1) { UNKNOWN }
): Int {
    if (array[n] == UNKNOWN) {
        array[n] = if (n < 2) {
            1
        } else {
            fibonacciMem(
                n - 2,
                array
            ) + fibonacciMem(n - 1, array)
        }
    }

    return array[n]
}

fun manhattanRec(i: Int, j: Int): Int {
    return if (i == 0 || j == 0) {
        1
    } else {
        manhattanRec(i - 1, j) + manhattanRec(
            i,
            j - 1
        )
    }
}

fun manhattanMem(i: Int, j: Int, array: Array<IntArray> = Array(i + 1) { IntArray(j + 1) { UNKNOWN } }): Int {

    if (array[i][j] == UNKNOWN) {
        array[i][j] = if (i == 0 || j == 0) {
            1
        } else {
            manhattanMem(
                i - 1,
                j,
                array
            ) + manhattanMem(i, j - 1, array)
        }
    }

    return array[i][j]
}

fun manhattanMemIter(i: Int, j: Int): Int {

    val array = Array(i + 1) { m ->
        IntArray(j + 1) { n ->
            if (n == 0 || m == 0) {
                1
            } else {
                0
            }
        }
    }

    for (m in 1 until array.size) {
        for (n in 1 until array[m].size) {
            array[m][n] = array[m - 1][n] + array[m][n - 1]
        }
    }

    return array[i][j]
}

fun main() {
    println(
        "testing fibonacci: ${fibonacciMem(23) == fibonacciRec(
            23
        )}"
    )
    println(
        "testing manhattan: ${manhattanRec(
            12,
            15
        ) == manhattanMem(12, 15)}"
    )
    println(
        "testing manhattan mem iter: ${manhattanMem(
            12,
            13
        ) == manhattanMemIter(12, 13)}"
    )
    val a = "razvan"
    val b = "razvcyahn"

    println("testing llcs mem rec: ${llcsRec(a, b) == llcsMemRec(a, b)}")
    println("testing llcs iter: ${llcsMemRec(a, b) == llcsIter(a, b)}")
    println("testing lcs: ${llcsIter(a, b) == lcs(a, b).length}")
}