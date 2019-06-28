package ro.razvan.uniud.puzzleboard.java.control;

import org.jetbrains.annotations.NotNull;
import puzzleboard.PuzzleBoard;

import static ro.razvan.uniud.puzzleboard.java.Principale.println;

import ro.razvan.uniud.puzzleboard.java.model.Board;

public final class PuzzleBoardPresenter {

    @NotNull
    private final PuzzleBoard gui;

    @NotNull
    private final Board board;

    public PuzzleBoardPresenter(@NotNull final Board board, @NotNull final PuzzleBoard gui) {
        this.gui = gui;
        this.board = board;
    }

    /**
     * Metodo che inizializza la configurazione della tavoletta
     */
    private void setArrangement() {
        println(board);

        final var array = board.getArrangement();

        for (int i = 0; i < array.length; i++) {

            final var element = array[i];

            final var coordinates = board.getRowAndColumn(i);
            gui.setNumber(coordinates.getFirstNonNull() + 1, coordinates.getSecondNonNull() + 1, element);
        }
    }

    /**
     * Mescola i tasselli della tavoletta
     */
    public void shuffle() {
        board.shuffle();
    }

    /**
     * Metodo che invoca l'inizializzazione della tavoletta e la visualizzazione degli elementi
     * Resta in attesa a leggere un nuovo click del mouse ed esegue le modifiche necesssarie
     */
    @SuppressWarnings("InfiniteLoopStatement")
    public void play() {
        setArrangement();
        gui.display();

        while (true) {

            final var k = gui.get();

            if (k != 0 && board.isMovable(k)) {

                board.moveTile(k);

                final var blank = board.blankSpacePosition();

                final var blankCoordinates = board.getRowAndColumn(blank);
                final var kCoordinates = board.getRowAndColumn(board.findPosition(k));

                println(blankCoordinates);
                println(kCoordinates);

                gui.setNumber(kCoordinates.getFirstNonNull() + 1, kCoordinates.getSecondNonNull() + 1, k);
                gui.clear(blankCoordinates.getFirstNonNull() + 1, blankCoordinates.getSecondNonNull() + 1);

                println(board);

                gui.display();

            }

        }
    }

}
