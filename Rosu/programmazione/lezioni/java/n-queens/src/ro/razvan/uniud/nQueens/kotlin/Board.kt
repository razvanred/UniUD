package ro.razvan.uniud.nQueens.kotlin

import ro.razvan.uniud.intSList.IntSList

class Board {

    val size: Int
    val queens: Int

    private val cols: IntSList
    private val rows: IntSList
    private val d1: IntSList
    private val d2: IntSList

    constructor(size: Int) {
        queens = 0
        this.size = size

        cols = IntSList()
        rows = IntSList()
        d1 = IntSList()
        d2 = IntSList()
    }

    private constructor(
        size: Int,
        queens: Int,
        rows: IntSList,
        cols: IntSList,
        d1: IntSList,
        d2: IntSList
    ) {
        this.size = size
        this.queens = queens
        this.cols = cols
        this.rows = rows
        this.d1 = d1
        this.d2 = d2
    }

    /**
     * La posizione ([i], [j]) della scacchiera sarà sotto attacco?
     */
    fun isUnderAttack(i: Int, j: Int): Boolean {
        return rows.contains(i) ||
                cols.contains(j) ||
                d1.contains(i - j) ||
                d2.contains(i + j)
    }

    fun addQueen(i: Int, j: Int): Board {
        return Board(
            size,
            queens + 1,
            rows.cons(i),
            cols.cons(j),
            d1.cons(i - j),
            d2.cons(i + j)
        )
    }

    val arrangement: String
        get() {
            val builder = StringBuilder("< $size, $queens, $rows, $cols, $d1, $d2, \" ")

            var curCols = cols
            var curRows = rows

            while (!curCols.isNull) {
                builder.append((curCols.car + 96).toChar())
                    .append(curRows.car.toChar())
                    .append(" ")

                curCols = curCols.cdr
                curRows = curRows.cdr
            }

            return builder.append(" \"").toString()
        }

    companion object {

        /**
         * Cerca se l'elemento [n] è presente nella lista
         *
         * @return true se l'elemento è presente nella lista
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