package ro.razvan.uniud.nQueens.kotlin

object Queens {

    fun numberOfSolutions(n: Int): Int = numberOfCompletions(Board(n))

    private fun numberOfCompletions(board: Board): Int {

        val size = board.size
        val queens = board.queens

        if (size == queens) {
            return 1
        }

        val i = queens + 1
        var count = 0

        for (j in 1..size) {

            if (!board.isUnderAttack(i, j)) {
                count += numberOfCompletions(board.addQueen(i, j))
            }

        }

        return count
    }

    @JvmStatic
    fun main(args: Array<String>) {
        println(numberOfSolutions(1) == 1)
        println(numberOfSolutions(2) == 0)
        println(numberOfSolutions(3) == 0)
        println(numberOfSolutions(4) == 2)
        println(numberOfSolutions(5) == 10)
        println(numberOfSolutions(6) == 4)
        println(numberOfSolutions(7) == 40)
        println(numberOfSolutions(8) == 92)
        println(numberOfSolutions(9) == 352)
        println(numberOfSolutions(10) == 724)
    }

}