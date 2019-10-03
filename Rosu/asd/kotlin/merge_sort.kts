/**
 * Razvan Rosu <razvanred.work@gmail.com>
 *
 * MergeSort di un array di interi.
 */

fun Array<Int>.mergeSort() {
    for (j in 1 until size) {
        val k = this[j]
        var i = j - 1
        while (i >= 0 && this[i] > k) {
            this[i + 1] = this[i]
            i--
        }
        this[i + 1] = k
    }
}

println(arrayOf(3, 2, 1, 5, 3, 7, 9, 0).apply { mergeSort() }.contentToString())
