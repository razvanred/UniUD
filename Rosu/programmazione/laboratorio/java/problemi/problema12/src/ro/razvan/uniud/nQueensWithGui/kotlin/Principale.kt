package ro.razvan.uniud.nQueensWithGui.kotlin

import queens.ChessboardView
import ro.razvan.uniud.nQueensWithGui.kotlin.Queens.displaySolutions

fun main(args: Array<String>) {

    val size = args[0].toInt()

    val gui = ChessboardView(size)
    val solutions = Queens.listOfAllSolutions(size)

    gui.displaySolutions(solutions)

}