package ro.razvan.uniud.puzzleboard.kotlin.util

import kotlin.math.log10

/**
 * Trova il numero delle cifre da cui è composto il numero
 */
val Int.numberOfDigits: Int
    get() = (log10(toDouble()) + 1).toInt()
