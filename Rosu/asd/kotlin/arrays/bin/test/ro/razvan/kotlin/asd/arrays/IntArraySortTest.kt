package ro.razvan.kotlin.asd.arrays

import org.junit.jupiter.api.Test

class IntArraySortTest {

    private val unorderedArray
        get() = intArrayOf(0, 3, 2, 1, 9, 10, 7, 5)

    @Test
    fun selectionSortTest() {
        with(unorderedArray) {
            selectionSort()
            checkOrderedArray()
        }
    }

    @Test
    fun insertionSortTest() {
        with(unorderedArray){
            insertionSort()
            checkOrderedArray()
        }
    }

    @Test
    fun insertionSortSelectionSortTest() {
        val selection = unorderedArray
        val insertion = unorderedArray
        selection.selectionSort()
        insertion.insertionSort()
        assert(insertion.contentEquals(selection))
    }

    private fun IntArray.checkOrderedArray() {
        assert(this[0] == 0)
        assert(this[1] == 1)
        assert(this[2] == 2)
        assert(this[3] == 3)
        assert(this[4] == 5)
        assert(this[5] == 7)
        assert(this[6] == 9)
        assert(this[7] == 10)
    }

}