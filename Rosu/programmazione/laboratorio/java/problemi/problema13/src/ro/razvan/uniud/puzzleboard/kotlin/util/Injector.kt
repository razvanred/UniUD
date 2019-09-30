package ro.razvan.uniud.puzzleboard.kotlin.util

import ro.razvan.uniud.puzzleboard.kotlin.control.PuzzleBoardPresenter
import ro.razvan.uniud.puzzleboard.kotlin.model.Board
import ro.razvan.uniud.puzzleboard.kotlin.ui.PuzzleBoard

class Injector private constructor(private val arrangement: IntArray) {

    private var board: Board? = null
    private var gui: PuzzleBoard? = null
    private var console: PuzzleBoardPresenter? = null

    companion object {

        private var localInstance: Injector? = null

        /**
         * Ritorna l'istanza della classe Injector
         */
        private val INSTANCE: Injector
            get() = localInstance
                ?: throw RuntimeException("The injector should be instantiated first with the start() method")

        /**
         * Istanzia un nuovo iniettore e lo assegna a INSTANCE
         *
         * @param arrangement posizioni dei tasselli all'interno della tavoletta
         */
        fun start(arrangement: IntArray) {
            if (localInstance == null) {
                localInstance = Injector(arrangement)
            }
        }

        fun start(size: Int) {
            start(IntArray(size * size) { it + 1 })
        }

        /**
         * Ritorna l'istanza di Board
         * Lazy Instantiation (Board viene istanziato in memoria solamente al momento del bisogno ed una sola volta)
         */
        private val board: Board
            get() {

                val instance = INSTANCE

                if (instance.board == null) {
                    instance.board = Board.Builder()
                        .setArrangement(instance.arrangement)
                        .build()
                }

                return instance.board!!
            }

        /**
         * Istanza della schermata grafica
         */
        private val gui: PuzzleBoard
            get() {

                val instance = INSTANCE

                if (instance.gui == null) {
                    instance.gui = PuzzleBoard(board.n)
                }

                return instance.gui!!
            }

        /**
         * Istanza del controller della schermata grafica e del Board
         */
        val console: PuzzleBoardPresenter
            get() {

                val instance = INSTANCE

                if (instance.console == null) {
                    instance.console = PuzzleBoardPresenter(gui, board)
                }

                return instance.console!!
            }

    }

}