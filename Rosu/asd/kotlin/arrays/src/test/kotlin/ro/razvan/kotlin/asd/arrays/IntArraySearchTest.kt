package ro.razvan.kotlin.asd.arrays

import org.junit.jupiter.api.Test

class IntArraySearchTest {

    private val orderedArray
        get() = IntArray(5) { it + 1 }

    @Test
    fun binarySearchTest() {
        with(orderedArray) {
            println(contentToString())
            assert(binarySearch(1) == 0)
            assert(binarySearch(2) == 1)
            assert(binarySearch(3) == 2)
            assert(binarySearch(4) == 3)
            assert(binarySearch(5) == 4)
        }
    }

}