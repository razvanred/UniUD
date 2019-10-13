package ro.razvan.kotlin.asd.arrays

fun IntArray.checkOrderedArray(): Boolean {
    for (i in 0 until size - 1) {
        if (this[i] > this[i + 1]) {
            return false
        }
    }
    return true
}

fun IntArray.binarySearch(x: Int): Int {
    check(checkOrderedArray())

    fun IntArray.search(x: Int): Int {
        val index = (size - 1) / 2
        return when {
            this[index] == x -> index
            this[index] < x -> copyOfRange(index + 1, size).search(x) + index + 1
            else -> copyOfRange(0, index).search(x)
        }
    }

    return search(x)
}

/**
 * @param a vettore immutabile di interi da valutare
 * @param k elemento da cercare
 */
fun ricercaDicotomica(a: IntArray, k: Int): Boolean {
    return ricercaDicotomica(a, k, 0, a.size - 1)
}

/**
 * @param a vettore immutabile di interi da controllare
 * @param k numero intero da trovare all'interno di a
 * @param p indice (incluso) da dove parte il sotto-array da valutare
 * @param q indice (incluso) dove termina il sotto-array da valutare
 */
private fun ricercaDicotomica(a: IntArray, k: Int, p: Int, q: Int): Boolean {
    val index = (p + q) / 2
    val found = a[index] == k

    return if (found || (q - p) <= 0) {
        found
    } else if (a[index] < k) {
        ricercaDicotomica(a, k, index + 1, q)
    } else {
        ricercaDicotomica(a, k, p, index - 1)
    }
}
