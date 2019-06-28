package ro.razvan.uniud.puzzleboard.kotlin.control

import ro.razvan.uniud.puzzleboard.kotlin.model.Board
import ro.razvan.uniud.puzzleboard.kotlin.ui.PuzzleBoard

class PuzzleBoardPresenter(
    private val gui: PuzzleBoard,
    private val board: Board
) {

    /**
     * Metodo che inizializza la configurazione della tavoletta
     */
    fun setArrangement() {
        println(board)

        board.arrangement.forEachIndexed { i, element ->
            val coordinates = board.getRowAndColumn(i)
            gui.setNumber(coordinates.first + 1, coordinates.second + 1, element)
        }
    }

    /**
     * Mescola i tasselli della tavoletta
     */
    fun shuffle() {
        board.shuffle()
    }

    /**
     * Invoca l'inizializzazione della tavoletta e la visualizzazione degli elementi
     * Resta in attesa a leggere un nuovo click del mouse ed esegue le modifiche necesssarie
     */
    fun play() {

        setArrangement()
        gui.display()

        while (true) {

            val k = gui.get()

            if (k != 0 && board.isMovable(k)) {

                board.moveTile(k)

                val blankPosition = board.blankSpacePosition

                val blankCoordinates = board.getRowAndColumn(blankPosition)
                val kCoordinates = board.getRowAndColumn(board.findPosition(k))

                println(blankCoordinates)
                println(kCoordinates)

                gui.setNumber(kCoordinates.first + 1, kCoordinates.second + 1, k)
                gui.clear(blankCoordinates.first + 1, blankCoordinates.second + 1)

                println(board)

                gui.display()

            }

        }

    }


}