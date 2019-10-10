package ro.razvan.uniud.lis.kotlin

import ro.razvan.uniud.lis.kotlin.LongestIncreasingSubsequence.lisRec
import ro.razvan.uniud.lis.kotlin.LongestIncreasingSubsequence.lisTopDown
import ro.razvan.uniud.lis.kotlin.LongestIncreasingSubsequence.llisRec
import ro.razvan.uniud.lis.kotlin.LongestIncreasingSubsequence.llisTopDown
import ro.razvan.uniud.lis.kotlin.LongestIncreasingSubsequence.llisTopDownDebugInits

fun main() {

    val array0 = intArrayOf(5, 4, 3, 2, 1)
    val array1 = intArrayOf(47, 38, 39, 25, 44)
    val array2 = intArrayOf(27, 90, 7, 29, 49, 8, 53, 1, 28, 6)
    val array3 = intArrayOf(9, 46, 54, 71, 60, 47, 0, 32, 25, 61)
    val array4 = intArrayOf(54, 52, 42, 33, 14, 40, 37, 61, 53, 1)

    println("--- Test Parte 1 ---")
    println(llisTopDown(array0) == llisRec(array0))
    println(llisTopDown(array1) == llisRec(array1))
    println(llisTopDown(array2) == llisRec(array2))
    println(llisTopDown(array3) == llisRec(array3))
    println(llisTopDown(array4) == llisRec(array4))

    println()

    println("--- Test Parte 2 ---")
    println("* LIS Recursive *")
    println(llisTopDown(array0) == lisRec(array0).length)
    println(llisTopDown(array1) == lisRec(array1).length)
    println(llisTopDown(array2) == lisRec(array2).length)
    println(llisTopDown(array3) == lisRec(array3).length)
    println(llisTopDown(array4) == lisRec(array4).length)

    println("* LIS Top Down == LIS Recursive *")
    println(lisTopDown(array0) == lisRec(array0))
    println(lisTopDown(array1) == lisRec(array1))
    println(lisTopDown(array2) == lisRec(array2))
    println(lisTopDown(array3) == lisRec(array3))
    println(lisTopDown(array4) == lisRec(array4))

    println()

    println("--- Test Parte 3 ---")
    println(llisTopDownDebugInits(array0))
    println(llisTopDownDebugInits(array1))
    println(llisTopDownDebugInits(array2))
    println(llisTopDownDebugInits(array3))
    println(llisTopDownDebugInits(array4))

}