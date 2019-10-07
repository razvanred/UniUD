package ro.razvan.kotlin.asd.arrays

@Visi
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
