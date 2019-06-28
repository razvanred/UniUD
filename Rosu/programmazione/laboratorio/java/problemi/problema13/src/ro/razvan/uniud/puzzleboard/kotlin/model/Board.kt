package ro.razvan.uniud.puzzleboard.kotlin.model

import ro.razvan.uniud.puzzleboard.DefaultValues
import ro.razvan.uniud.puzzleboard.kotlin.util.numberOfDigits
import java.lang.RuntimeException
import kotlin.math.pow
import kotlin.math.sqrt
import kotlin.random.Random

class Board private constructor(
    val n: Int,
    val arrangement: IntArray
) {

    /**
     * Ritorna true se le caselle sono in ordine
     */
    val areTilesSorted: Boolean
        get() {
            IntArray(n) { it + 1 }.forEach { i ->
                if (arrangement[i - 1] != i) {
                    return false
                }
            }
            return true
        }

    /**
     * Verifica se l'elemento all'interno della tavoletta può essere spostato
     *
     * Se la colonna è pari a 0 non ci sarà alcun elemento destro
     * Se la colonna è pari a n - 1 non ci sarà alcun elemento sinistro
     *
     * Se la riga è pari a 0 non ci sarà alcun elemeno superiore
     * Se la riga è pari a n - 1 non ci sarà alcun elemento inferiore
     *
     * @param value elemento da spostare
     * @return true se l'elemento è spostabile
     */
    fun isMovable(value: Int): Boolean {

        if (n <= 1) {
            return false
        }

        val position = findPosition(value)

        val coordinates = getRowAndColumn(position)

        val row = coordinates.first
        val column = coordinates.second

        val bottom = if (row == n - 1) {
            -1
        } else {
            arrangement[position + n]
        }

        val top = if (row == 0) {
            -1
        } else {
            arrangement[position - n]
        }

        val left = if (column == 0) {
            -1
        } else {
            arrangement[position - 1]
        }

        val right = if (column == n - 1) {
            -1
        } else {
            arrangement[position + 1]
        }

        val size = n * n

        return right == size || left == size || top == size || bottom == size
    }

    override fun toString(): String {

        val builder = StringBuilder()
        val digits = (n * n).numberOfDigits

        for (i in 0 until n * n step n) {
            for (j in 0 until n) {
                builder.append(
                    if (arrangement[i + j] == n * n) {
                        " ".repeat(digits)
                    } else {
                        String.format("%0${digits}d", arrangement[i + j])
                    }
                ).append("\t")
            }
            builder.append("\n\n")
        }

        return builder.toString()
    }

    /**
     * Trova la posizione di un determinato tassello all'interno dell'array arrangements
     *
     * @param value valore del tassello
     * @return posizione del tassello
     */
    fun findPosition(value: Int): Int = arrangement.indexOfFirst { it == value }

    /**
     * Trova la riga e la colonna di un elemento all'interno dell'array arrangements
     *
     * @param position posizione di un'elemento all'interno dell'array arrangements
     * @return coordinate del tassello (partendo da 0)
     */
    fun getRowAndColumn(position: Int) = Pair(
        IntArray(n) { (it + 1) * n }
            .indexOfFirst { position < it },
        position % n
    )

    /**
     * Verifica se è possibile muovere un determinato tassello e muove il tassello
     *
     * @param value valore del tassello da muovere
     * @return true se il tassello è stato mosso con successo
     */
    fun moveTile(value: Int): Boolean {

        if (n <= 1) {
            return false
        }

        val position = arrangement.indexOfFirst { it == value }

        val coordinates = getRowAndColumn(position)

        val row = coordinates.first
        val column = coordinates.second

        val topPosition = position - n
        val bottomPosition = position + n
        val leftPosition = position - 1
        val rightPosition = position + 1

        val top = if (row == 0) {
            -1
        } else {
            arrangement[topPosition]
        }

        val bottom = if (row == n - 1) {
            -1
        } else {
            arrangement[bottomPosition]
        }

        val left = if (column == 0) {
            -1
        } else {
            arrangement[leftPosition]
        }

        val right = if (column == n - 1) {
            -1
        } else {
            arrangement[rightPosition]
        }

        val pair = when (n * n) {

            left -> Pair(leftPosition, left)

            right -> Pair(rightPosition, right)

            top -> Pair(topPosition, top)

            bottom -> Pair(bottomPosition, bottom)

            else -> return false
        }

        arrangement[pair.first] = value
        arrangement[position] = pair.second

        return true
    }

    /**
     * Mescola il contenuto della tavoletta eseguendo 500 mosse legali
     */
    fun shuffle() {
        for (i in 0 until 500) {
            val movables = arrangement.filter(::isMovable)
            moveTile(movables[Random.nextInt(movables.size)])
        }
    }

    /**
     * Posizione del tassello vuoto nell'array arrangements
     */
    val blankSpacePosition: Int
        get() = findPosition(n * n)

    /**
     * Classe che permette di costruire la nuova Board
     */
    class Builder {

        private var arrangement: IntArray? = null
        private var n: Int = -1

        /**
         * Imposta la dimensione di un lato della tavoletta quadrata
         *
         * @param n dimensione del lato del quadrato
         */
        fun setSize(n: Int): Builder {
            this.n = n
            return this
        }

        /**
         * Imposta l'ordinamento delle arrangement all'interno della tavoletta
         *
         * @param tiles disposizione delle arrangement
         */
        fun setArrangement(tiles: IntArray): Builder {
            this.arrangement = tiles
            setSize(sqrt(tiles.size.toDouble()).toInt())
            return this
        }

        /**
         * Costruisce e ritorna una nuova istanza di Board
         *
         * @return istanza di Board configurata
         * @throws IllegalArgumentException se n e arrangement sono incompatibili tra loro
         * @throws IllegalArgumentException se arrangement non contiene tutti i numeri da 1 a n inclusi
         */
        fun build(): Board {

            val arrangement = arrangement ?: throw RuntimeException("arrangement has not been initialized")

            if (n.toDouble().pow(2).toInt() != arrangement.size) {
                throw IllegalArgumentException("The given N and the given arrangement of arrangement are non-compatible")
            }

            IntArray(n) { it + 1 }.forEach { element ->
                if (!arrangement.contains(element)) {
                    throw IllegalArgumentException("The tile $element is missing")
                }
            }

            return Board(n, arrangement)
        }

    }

}