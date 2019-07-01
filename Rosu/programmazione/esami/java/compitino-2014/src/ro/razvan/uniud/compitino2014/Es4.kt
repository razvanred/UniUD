package ro.razvan.uniud.compitino2014

import java.util.stream.IntStream

object Es4 {

    @JvmStatic
    fun main(args: Array<String>) {

    }

    class DistributoreAutomatico {

        enum class Moneta(val value: Int) {
            CINQUE(5),
            DIECI(10),
            VENTI(20),
            CINQUANTA(50),
            CENTO(100),
            DUECENTO(200)
        }

        private val monete = IntStream.generate { 0 }.limit(Moneta.values().size.toLong()).toArray()

        fun introduciMonete(n: Int, v: Moneta) {
            monete[v.ordinal] += n
        }

    }

}