package ro.razvan.uniud.stringSList.kotlin

object BtrNotation {

    private const val PLUS_CHARACTER = '+'
    private const val MINUS_CHARACTER = '-'
    private const val ZERO_CHARACTER = '.'

    fun btrSucc(btr: String): String {
        val n = btr.length
        val lsb = btr.last()

        return if (n == 1) {
            when (lsb) {
                PLUS_CHARACTER -> PLUS_CHARACTER.toString() + MINUS_CHARACTER
                MINUS_CHARACTER -> ZERO_CHARACTER.toString()
                else -> PLUS_CHARACTER.toString()
            }
        } else {
            val pre = btr.substring(0, n - 1)

            if (lsb == PLUS_CHARACTER) {
                btrSucc(pre) + MINUS_CHARACTER
            } else {
                pre + if (lsb == MINUS_CHARACTER) {
                    ZERO_CHARACTER
                } else {
                    PLUS_CHARACTER
                }
            }
        }
    }

    fun btrSucc(btr: String, n: Int): StringSList {
        var list = StringSList(btr)
        var i = 0

        while (++i < n) {
            list = list.cons(btrSucc(list.listRef(0)))
        }

        return list.reverse()
    }

}