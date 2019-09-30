package ro.razvan.uniud.nextBtr

object BtrNotation {

    private const val PLUS_CHARACTER = '+'
    private const val MINUS_CHARACTER = '-'
    private const val ZERO_CHARACTER = '.'

    fun btrSucc(btr: String): String {
        val n = btr.length
        val lsb = btr.last()

        return if (n == 1) {
            if (lsb == PLUS_CHARACTER) {
                PLUS_CHARACTER.toString() + MINUS_CHARACTER
            } else {
                PLUS_CHARACTER.toString()
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

}