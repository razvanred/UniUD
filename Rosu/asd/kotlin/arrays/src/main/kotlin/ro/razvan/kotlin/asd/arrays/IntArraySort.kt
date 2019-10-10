package ro.razvan.kotlin.asd.arrays

fun IntArray.selectionSort() {
    for (i in 0 until size) {
        var smallerKey = i
        for (j in i + 1 until size) {
            if (this[j] < this[smallerKey]) {
                smallerKey = j
            }
        }
        val smaller = this[smallerKey]
        this[smallerKey] = this[i]
        this[i] = smaller
    }
}

fun IntArray.insertionSort() {
    for (j in 1 until size) {
        val key = this[j]
        var i = j - 1
        while (i >= 0 && this[i] > key) {
            this[i + 1] = this[i]
            i--
        }
        this[i + 1] = key
    }
}