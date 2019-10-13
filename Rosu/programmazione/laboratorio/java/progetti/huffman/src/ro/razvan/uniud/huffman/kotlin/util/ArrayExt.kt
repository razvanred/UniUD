package ro.razvan.uniud.huffman.kotlin.util

/**
 * Metodo ispirato ad Arrays.deepToString()
 *
 * @param <T> tipo degli elementi dell'array
 * @return codifica testuale dell'array
 */
fun <T> Array<T>.deepToString(): String {

    val builder = StringBuilder("[")

    for (i in 0 until size) {
        builder.append(this[i])

        if (i < size - 1) {
            builder.append(", ")
        }
    }

    return builder.append("]").toString()
}