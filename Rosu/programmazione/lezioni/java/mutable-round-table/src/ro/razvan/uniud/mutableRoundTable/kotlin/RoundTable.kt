package ro.razvan.uniud.mutableRoundTable.kotlin

import ro.razvan.uniud.intSList.IntSList

class RoundTable {

    companion object {

        private fun rangeRec(start: Int, end: Int): IntSList {
            return if (start > end) {
                IntSList()
            } else {
                IntSList(start).append(rangeRec(start + 1, end))
            }
        }

        private fun rangeIter(start: Int, end: Int): IntSList {
            var list = IntSList()
            var i = end

            while (i >= start) {
                list = IntSList(i, list)
                i--
            }

            return list
        }

        fun count(n: Int): Int {
            val roundTable = RoundTable(n)
            var i = n
            while (i-- > 1) {
                roundTable.serveKnightAndHandPitcher()
            }

            return roundTable.knightWithPitcher
        }

    }

    var knightsLeft: IntSList
    private var knightsStanding: IntSList
    private var served: Boolean

    val knightServed: Boolean
        get() = served

    val knightWithPitcher: Int
        get() = knightsLeft.car

    val knightsAtTable: Int
        get() = knightsLeft.length

    constructor(n: Int) {
        knightsLeft = rangeIter(1, n)
        served = false
        knightsStanding = IntSList.NULL_INT_S_LIST
    }

    fun serveKnight() {
        knightsLeft = knightsLeft.cdr.cdr.cons(knightsLeft.car)
        knightsStanding = IntSList(knightsLeft.cdr.car, knightsStanding)
        served = true
    }

    fun handPitcher() {
        knightsLeft = knightsLeft.cdr.append(IntSList(knightsLeft.car))
        served = false
    }

    fun serveKnightAndHandPitcher() {
        knightsStanding = IntSList(knightsLeft.cdr.car, knightsStanding)
        knightsLeft = knightsLeft.cdr.cdr.append(IntSList(knightsLeft.car))
        served = false
    }


}