package ro.razvan.uniud.onesComplement

object IterationStrings {

    private const val ZERO_CHAR = '0'
    private const val ONE_CHAR = '1'

    private fun bitComplement(bit: Char): Char = when (bit) {
        ZERO_CHAR -> ONE_CHAR
        ONE_CHAR -> ZERO_CHAR
        else -> throw IllegalArgumentException("la stringa non è binaria")
    }

    fun onesComplement(bin: String): String {
        val builder = StringBuilder()

        bin.map(::bitComplement)
            .forEach { builder.append(it) }

        return builder.toString()
    }

}