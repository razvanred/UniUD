package ro.razvan.uniud.eratosthenes.kotlin


object Eratosthenes {

    /**
     * Calcola tutti i numeri primo fino a [n]
     * Pre: [n] > 1
     *
     * @return tutti i numeri primi minori o uguali a [n]
     */
    fun primesUpTo(n: Int): List<Int> {

        val sieve = BooleanArray(n + 1) { i ->
            i >= 2
        }
        val primes = ArrayList<Int>()
        var p = 2

        /* Term: n + 1 - p */
        while (p <= n) {

            if (sieve[p]) {

                primes.add(p)
                var i = 2 * p

                while (i <= n) {
                    sieve[i] = false
                    i += p
                }

            }

            p++
        }

        return primes // Post: primes = { primi <= n }
    }

    @JvmStatic
    fun main(args: Array<String>) {
        println(primesUpTo(args[0].toInt()))
    }

}