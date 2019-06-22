package ro.razvan.uniud.nQueens.kotlin

import ro.razvan.uniud.intSList.IntSList

class Board {

    val queens: Int
    val size: Int

    private val columns: IntSList
    private val rows: IntSList
    private val d1: IntSList
    private val d2: IntSList

    constructor(size: Int) {
        this.size = size
        queens = 0

        columns = IntSList()
        rows = IntSList()
        d1 = IntSList()
        d2 = IntSList()
    }

    private constructor(
        size: Int,
        queens: Int,
        rows: IntSList,
        columns: IntSList,
        d1: IntSList,
        d2: IntSList
    ) {
        this.size = size
        this.queens = queens
        this.rows = rows
        this.columns = columns
        this.d1 = d1
        this.d2 = d2
    }

    /**
     * Codifica testuale della configurazione
     */
    val arragement: String
        get() {

            val builder = StringBuilder("< ")
                .append(size).append(", ")
                .append(queens).append(", ")
                .append(rows).append(", ")
                .append(columns).append(", ")
                .append(d1).append(", ")
                .append(d2).append(", ")
                .append("\" ")

            var curColumns = columns
            var curRows = rows

            while (!curColumns.isNull) {

                builder.append((curColumns.car + 96).toChar())
                    .append(curRows.car)
                    .append(" ")

                curRows = curRows.cdr
                curColumns = curColumns.cdr
            }

            return builder.append("\" >").toString()
        }

    /**
     * La posizione ([i], [j]) sarà sotto assedio
     */
    fun isUnderAttack(i: Int, j: Int): Boolean {
        return rows.contains(i) ||
                columns.contains(j) ||
                d1.contains(i - j) ||
                d2.contains(i + j)
    }

    fun addQueen(i: Int, j: Int): Board =
        Board(size, queens + 1, rows.cons(i), columns.cons(j), d1.cons(i - j), d2.cons(i + j))

    companion object {

        /**
         * Verifica se [n] è presente nella lista
         *
         * @return true se [n] è presente nella lista
         */
        private fun IntSList.contains(n: Int): Boolean {

            var list = this

            while (!list.isNull) {

                if (list.car == n) {
                    return true
                }

                list = list.cdr
            }

            return false
        }

    }

}