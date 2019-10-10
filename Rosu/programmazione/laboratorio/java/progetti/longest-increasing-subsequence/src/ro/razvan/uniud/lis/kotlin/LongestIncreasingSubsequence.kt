package ro.razvan.uniud.lis.kotlin

import ro.razvan.uniud.intSList.IntSList
import kotlin.math.max

object LongestIncreasingSubsequence {

    private const val UNKNOWN = -1

    /**
     * Data una sequenza s di n interi positivi rappresentata da un array,
     * calcola la lunghezza della più lunga sottosequenza di s strettamente
     * crescente
     *
     * @param s sequenza di interi positivi
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    fun llisRec(s: IntArray): Int = llisRec(s, 0, 0)

    /**
     * Caso base: la coda è vuota, quindi con [i] == [s].length
     * <p>
     * Gli elementi della sottosequenza devono essere strettamente maggiori di [t]
     * <p>
     * Se [s] [[i]] non è maggiore di t non può far parte della sottosequenza,
     * quindi ci si riconduce direttamente al problema per una coda più corta, partendo da [i] + 1.
     * <p>
     * Altrimenti, [s] [[i]] può far parte della sottosequenza più lunga.
     * Se vi fa parte, i successivi elementi (da cercare nella coda che inizia in posizione [i]+1)
     * devono essere maggiori di [s] [[i]].
     * Se non vi fa parte il vincolo aggiuntivo resta determinato da [t], e si sceglierà l'opzione
     * più favorevole.
     *
     * @param s array di elementi
     * @param i indice di partenza della sottosequenza
     * @param t elemento che deve essere strettamente più grande di [s] [[i]]
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    private fun llisRec(s: IntArray, i: Int, t: Int): Int {

        return when {
            s.size == i -> 0

            s[i] <= t -> llisRec(s, i + 1, t)

            else -> max(
                1 + llisRec(s, i + 1, s[i]),
                llisRec(s, i + 1, t)
            )
        }
    }

    /**
     * Versione più efficiente del metodo llisRec
     *
     * @param s sequenza di interi positivi
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    fun llisTopDown(s: IntArray): Int =
        llisTopDown(s, 0, 0, Array(s.size + 1) {
            IntArray(s.size + 1) { UNKNOWN }
        })

    /**
     * @param s      sequenza di interi positivi
     * @param i      indice di partenza della sottosequenza
     * @param j      indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @param matrix stato dell'elaborazione
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    private fun llisTopDown(s: IntArray, i: Int, j: Int, matrix: Array<IntArray>): Int {

        if (matrix[i][j] == UNKNOWN) {

            matrix[i][j] = if (s.size == i) {
                0
            } else {

                val t = if (j == 0) 0 else s[j - 1]

                if (s[i] <= t) {
                    llisTopDown(s, i + 1, j, matrix)
                } else {
                    max(
                        1 + llisTopDown(s, i + 1, i + 1, matrix),
                        llisTopDown(s, i + 1, j, matrix)
                    )
                }

            }
        }

        return matrix[i][j]
    }

    /**
     * Versione del metodo llisTopDown che ritorna in un oggetto Pair la lunghezza della sottosequenza crescente
     * più lunga e il numero di inizializzazioni eseguite dal metodo sulle celle della matrice matrix
     *
     * @param s sequenza di interi positivi
     * @return oggetto Pair contente la lunghezza della sottosequenza crescente e il numero di inizializzazioni
     * eseguite dal metodo sulle celle della matrice matrix
     */
    fun llisTopDownDebugInits(s: IntArray): Pair<Int, Int> =
        llisTopDownDebugInits(s, 0, 0, Array(s.size + 1) {
            IntArray(s.size + 1) { UNKNOWN }
        })

    /**
     * Versione del metodo llisTopDown che ritorna in un oggetto Pair la lunghezza della sottosequenza crescente
     * più lunga e il numero di inizializzazioni eseguite dal metodo sulle celle della matrice matrix
     *
     * @param s      sequenza di interi positivi
     * @param i      indice di partenza della sottosequenza da esaminiare
     * @param j      indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @param matrix stato dell'elaborazione
     * @return oggetto Pair contente la lunghezza della sottosequenza crescente e il numero di inizializzazioni
     * eseguite dal metodo sulle celle della matrice matrix
     */
    private fun llisTopDownDebugInits(s: IntArray, i: Int, j: Int, matrix: Array<IntArray>): Pair<Int, Int> {

        val counter: Int

        if (matrix[i][j] == UNKNOWN) {

            if (s.size == i) {

                counter = 1
                matrix[i][j] = 0

            } else {

                val t = if (j == 0) {
                    0
                } else {
                    s[j - 1]
                }

                if (s[i] <= t) {

                    val pair = llisTopDownDebugInits(s, i + 1, j, matrix)

                    matrix[i][j] = pair.first
                    counter = pair.second + 1

                } else {

                    val left = llisTopDownDebugInits(s, i + 1, i + 1, matrix)
                    val right = llisTopDownDebugInits(s, i + 1, j, matrix)

                    val firstSize = left.first + 1
                    val secondSize = right.first

                    matrix[i][j] = if (firstSize > secondSize) {
                        firstSize
                    } else {
                        secondSize
                    }

                    counter = left.second + right.second + 1
                }

            }

        } else {
            counter = 0
        }

        return Pair(matrix[i][j], counter)
    }

    /**
     * Longest Increasing Sub-sequence
     *
     * @param s sequenza di interi positivi
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    fun lisRec(s: IntArray): IntSList = lisRec(s, 0, 0)

    /**
     * Longest Increasing Sub-sequence
     *
     * @param s sequenza di interi positivi
     * @param i indice di partenza della sottosequenza da esaminare
     * @param t indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    private fun lisRec(s: IntArray, i: Int, t: Int): IntSList = when {

        s.size == i -> IntSList.NULL_INT_S_LIST

        s[i] <= t -> lisRec(s, i + 1, t)

        else -> {
            val left = IntSList(s[i], lisRec(s, i + 1, s[i]))
            val right = lisRec(s, i + 1, t)

            if (max(left.length, right.length) == left.length) {
                left
            } else {
                right
            }
        }

    }

    /**
     * LIS con la tecnica Top-Down applicata
     *
     * @param s sequenza di interi positivi
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    fun lisTopDown(s: IntArray): IntSList =
        lisTopDown(s, 0, 0, Array(s.size + 1) { arrayOfNulls<IntSList>(s.size + 1) })

    /**
     * LIS con la tecnica Top-Down applicata
     *
     * @param s      sequenza di interi positivi
     * @param i      indice di partenza della sottosequenza da esaminare
     * @param j      indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @param matrix stato dell'elaborazione
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    private fun lisTopDown(s: IntArray, i: Int, j: Int, matrix: Array<Array<IntSList?>>): IntSList {

        if (matrix[i][j] == null) {

            matrix[i][j] = if (s.size == i) {
                IntSList.NULL_INT_S_LIST
            } else {

                val t = if (j == 0) {
                    0
                } else {
                    s[j - 1]
                }

                if (s[i] <= t) {
                    lisTopDown(s, i + 1, j, matrix)
                } else {

                    val left = IntSList(s[i], lisTopDown(s, i + 1, i + 1, matrix))
                    val right = lisTopDown(s, i + 1, j, matrix)

                    if (max(left.length, right.length) == left.length) {
                        left
                    } else {
                        right
                    }
                }

            }
        }

        return matrix[i][j]!!
    }

}