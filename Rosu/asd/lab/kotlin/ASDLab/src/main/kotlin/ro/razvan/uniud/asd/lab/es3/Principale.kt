package ro.razvan.uniud.asd.lab.es3

fun main() {
    readLine()!!.suffixes().forEach(::println)
}

fun String.suffixes(): Array<String> =
    IntArray(length) { it }
        .map(::substring)
        .toTypedArray()