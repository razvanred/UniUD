package ro.razvan.uniud.asd.lab.es4

fun String.prefixes(): Array<String> =
    IntArray(length) { it }
        .map { this.substring(0, this.length - it) }
        .toTypedArray()