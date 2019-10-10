package ro.razvan.uniud.roundTable

class RoundTable {

    private val knightsAtTable: IntSList
    val knightsLeft: IntSList
    private val served: Boolean

    constructor(n: Int) {
        knightsLeft = IntSList.NULL_INT_S_LIST
        knightsAtTable = rangeIter(1, n)
        served = false
    }

    constructor(knightsAtTable: IntSList, knightsLeft: IntSList, served: Boolean) {
        this.knightsAtTable = knightsAtTable
        this.knightsLeft = knightsLeft
        this.served = served
    }

    val numKnights: Int
        get() = knightsAtTable.length

    val knightWithPitcher: Int
        get() = knightsAtTable.car

    val knightServed: Boolean
        get() = served

    val leftKnight: Int
        get() = knightsAtTable.cdr.car

    /**
     * Il cavaliere, dopo essere stato servito, lascia la tavola rotonda
     */
    fun serveKnight(): RoundTable {
        return RoundTable(
            knightsAtTable = knightsAtTable.cdr.cdr.cons(knightsAtTable.car),
            knightsLeft = knightsLeft.cons(knightsAtTable.cdr.car),
            served = true
        )
    }

    /**
     * Metodo che permette di passare la brocca al prossimo cavaliere
     */
    fun passPicher(): RoundTable {
        return RoundTable(
            knightsAtTable.cdr.append(IntSList(knightsAtTable.car)),
            knightsLeft,
            false
        )
    }

    /**
     * Servi il cavaliere e passa la brocca
     */
    fun serveKnightAndPassPitcher(): RoundTable {
        return RoundTable(
            knightsAtTable.cdr.cdr.append(IntSList(knightsAtTable.car)),
            knightsLeft.cons(knightsAtTable.cdr.car),
            false
        )
    }

    companion object {

        /**
         * Ritorna una IntSList contente i numeri che vanno da start a end inclusi
         * Modalità ricorsiva
         */
        private fun rangeRec(start: Int, end: Int): IntSList {
            return if (start > end) {
                IntSList.NULL_INT_S_LIST
            } else {
                IntSList(start, rangeRec(start + 1, end))
            }
        }

        /**
         * Ritorna una IntSList contente i numeri che vanno da start a end inclusi
         * Modalità iterattiva
         */
        private fun rangeIter(start: Int, end: Int): IntSList {
            var sList = IntSList()
            var i = start

            while (i <= end) {
                sList = sList.cons(i++)
            }

            return sList.reverse()
        }

        fun count(n: Int): Int {

            var roundTable = RoundTable(n)

            while (roundTable.numKnights > 1) {
                roundTable = roundTable.serveKnightAndPassPitcher()
            }

            return roundTable.knightWithPitcher
        }

    }

}