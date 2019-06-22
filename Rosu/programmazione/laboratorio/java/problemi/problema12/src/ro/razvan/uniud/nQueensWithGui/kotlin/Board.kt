package ro.razvan.uniud.nQueensWithGui.kotlin

import ro.razvan.uniud.generics.SList

class Board {

    val queens: Int
    val size: Int

    private val rows: SList<Int>
    private val columns: SList<Int>
    private val d1: SList<Int>
    private val d2: SList<Int>

    private constructor(
        size: Int,
        queens: Int,
        rows: SList<Int>,
        columns: SList<Int>,
        d1: SList<Int>,
        d2: SList<Int>
    ) {
        this.size = size
        this.queens = queens
        this.rows = rows
        this.columns = columns
        this.d1 = d1
        this.d2 = d2
    }

    constructor(size: Int) {
        this.size = size
        queens = 0
        rows = SList()
        columns = SList()
        d1 = SList()
        d2 = SList()
    }

    companion object {

        /**
         * Verifica se un elemento è presente all'interno della lista generica
         *
         * @param list    lista in cui bisogna cercare l'elemento
         * @param element elemento da cercare nella lista
         * @param <T>     il tipo della lista e dell'elemento da esaminare
         * @return true se l'elemento si trova nella lista
         */
        private fun <T> SList<T>.contains(element: T): Boolean {

            var list = this

            while (!list.isNull) {

                if (list.car() == element) {
                    return true
                }

                list = list.cdr()
            }

            return false
        }

    }

    /**
     * Aggiunge una nuova regina alla scacchiera nella posizione (i, j)
     *
     * @param i riga
     * @param j colonna
     * @return nuova scacchiera con una nuova regina aggiunta
     */
    fun addQueen(i: Int, j: Int) =
        Board(size, queens + 1, rows.cons(i), columns.cons(j), d1.cons(i - j), d2.cons(i + j))

    /**
     * Verifica se la posizione (i, j) è sotto attacco
     *
     * @param i riga
     * @param j colonna
     * @return true se la posizione è sotto attacco
     */
    fun isUnderAttack(i: Int, j: Int): Boolean = rows.contains(i) ||
            columns.contains(j) ||
            d1.contains(i - j) ||
            d2.contains(i + j)

    val representation: String
        get() {

            val builder = StringBuilder()
            var curColumns = columns
            var curRows = rows

            while (!curColumns.isNull) {

                builder.append((curColumns.car() + 96).toChar())
                    .append(curRows.car())
                    .append(" ")

                curColumns = curColumns.cdr()
                curRows = curRows.cdr()

            }

            return builder.toString()
        }

}