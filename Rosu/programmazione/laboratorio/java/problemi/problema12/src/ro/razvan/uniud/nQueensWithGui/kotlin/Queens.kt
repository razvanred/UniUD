package ro.razvan.uniud.nQueensWithGui.kotlin

import queens.ChessboardView
import ro.razvan.uniud.generics.SList

object Queens {

    val NULL_BOARD_LIST = SList<Board>()

    fun numberOfSolutions(n: Int): Int = numberOfCompletions(Board(n))

    private fun numberOfCompletions(board: Board): Int {

        if (board.queens == board.size) {
            return 1
        }

        var count = 0
        val i = board.queens + 1

        for (j in 1..board.size) {
            if (!board.isUnderAttack(i, j)) {
                count += numberOfCompletions(board.addQueen(i, j))
            }
        }

        return count
    }

    private fun listOfAllCompletions(board: Board): SList<Board> {

        if (board.queens == board.size) {
            return NULL_BOARD_LIST.cons(board)
        }

        val i = board.queens + 1
        var solutions = NULL_BOARD_LIST

        for (j in 1..board.size) {
            if (!board.isUnderAttack(i, j)) {
                solutions = solutions.append(listOfAllCompletions(board.addQueen(i, j)))
            }
        }

        return solutions
    }

    fun ChessboardView.displaySolutions(solutions: SList<Board>) {
        var list = solutions
        while (!list.isNull) {
            setQueens(list.car().representation)
            list = list.cdr()
        }
    }

    fun listOfAllSolutions(size: Int): SList<Board> {
        return listOfAllCompletions(Board(size))
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