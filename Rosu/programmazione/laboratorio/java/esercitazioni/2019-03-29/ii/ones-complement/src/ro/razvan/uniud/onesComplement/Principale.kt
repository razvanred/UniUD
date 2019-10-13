package ro.razvan.uniud.onesComplement

import ro.razvan.uniud.onesComplement.IterationStrings.onesComplement

fun main() {
    testing()
}

private fun testing() {
    println(onesComplement("1111") == "0000")
    println(onesComplement("101") == "010")
}