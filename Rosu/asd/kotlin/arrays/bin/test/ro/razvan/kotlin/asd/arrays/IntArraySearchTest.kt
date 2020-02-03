package ro.razvan.kotlin.asd.arrays

import org.junit.jupiter.api.Test

class IntArraySearchTest {

    private val orderedArray
        get() = intArrayOf(1, 2, 3, 4, 5)

    // numeri presenti all'interno dell'array
    @Test
    fun ricercaDicotomicaTest() {
        assert(ricercaDicotomica(orderedArray, 1))
        assert(ricercaDicotomica(orderedArray, 2))
        assert(ricercaDicotomica(orderedArray, 3))
        assert(ricercaDicotomica(orderedArray, 4))
        assert(ricercaDicotomica(orderedArray, 5))
    }

    // numeri più grandi a destra non presenti nell'array
    @Test
    fun ricercaDicotomicaNumeriADestraTest() {
        assert(!ricercaDicotomica(orderedArray, 6))
        assert(!ricercaDicotomica(orderedArray, 99))
    }

    // numeri più piccoli a sinistra non presenti nell'array
    @Test
    fun ricercaDicotomicaNumeriASinistraTest() {
        assert(!ricercaDicotomica(orderedArray, -1))
        assert(!ricercaDicotomica(orderedArray, -2456))
    }

}