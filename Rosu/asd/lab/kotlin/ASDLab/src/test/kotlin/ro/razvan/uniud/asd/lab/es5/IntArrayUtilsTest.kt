package ro.razvan.uniud.asd.lab.es5

import org.junit.Test

class IntArrayUtilsTest {

    private val orderedArray = IntArray(7) { it + 1 }

    @Test
    fun binarySearchFoundTest() {
        assert(orderedArray.binarySearch(1) == 0)
        assert(orderedArray.binarySearch(2) == 1)
        assert(orderedArray.binarySearch(3) == 2)
        assert(orderedArray.binarySearch(4) == 3)
        assert(orderedArray.binarySearch(5) == 4)
        assert(orderedArray.binarySearch(6) == 5)
        assert(orderedArray.binarySearch(7) == 6)
    }

    @Test
    fun binarySearchNotFoundTest() {
        assert(orderedArray.binarySearch(0) == -1)
        assert(orderedArray.binarySearch(-1) == -1)
        assert(orderedArray.binarySearch(-200) == -1)
        assert(orderedArray.binarySearch(8) == -1)
        assert(orderedArray.binarySearch(9) == -1)
        assert(orderedArray.binarySearch(200) == -1)
    }

}