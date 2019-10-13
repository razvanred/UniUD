package ro.razvan.uniud.puzzleboard.java.util;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import puzzleboard.PuzzleBoard;
import ro.razvan.uniud.puzzleboard.java.control.PuzzleBoardPresenter;
import ro.razvan.uniud.puzzleboard.java.model.Board;

import java.util.Objects;
import java.util.stream.IntStream;

public final class Injector {

    @Nullable
    private static Injector INSTANCE = null;

    @NotNull
    private final int[] arrangement;

    @Nullable
    private Board board;

    @Nullable
    private PuzzleBoard gui;

    @Nullable
    private PuzzleBoardPresenter console;

    private Injector(@NotNull final int[] arrangement) {
        this.arrangement = arrangement;
    }

    /**
     * Istanzia un nuovo iniettore e lo assegna a INSTANCE
     *
     * @param arrangement posizioni dei tasselli all'interno della tavoletta
     */
    public static void start(@NotNull final int[] arrangement) {
        if (INSTANCE == null) {
            INSTANCE = new Injector(arrangement);
        }
    }

    public static void start(final int size) {
        start(IntStream.rangeClosed(1, size * size).toArray());
    }

    /**
     * Ritorna l'istanza della classe Injector
     *
     * @return iniettore di dipendenze
     * @throws RuntimeException se l'iniettore non è stato inizializzato
     */
    @NotNull
    private static Injector getINSTANCE() throws RuntimeException {

        if (INSTANCE == null) {
            throw new RuntimeException("The injector should be instantiated first with the start() method");
        }

        return Objects.requireNonNull(INSTANCE);
    }

    /**
     * Il metodo ritorna l'istanza di Board
     * Lazy Instantiation (Board viene istanziato in memoria solamente al momento del bisogno ed una sola volta)
     *
     * @return istanza di Board
     */
    @NotNull
    private static Board getBoard() {

        final var instance = getINSTANCE();

        if (instance.board == null) {
            instance.board = new Board.Builder()
                    .setArrangement(instance.arrangement)
                    .build();
        }

        return Objects.requireNonNull(instance.board);
    }

    /**
     * Ritorna la schermata grafica del gioco
     *
     * @return gui del gioco
     */
    @NotNull
    private static PuzzleBoard getGui() {
        final var instance = getINSTANCE();

        if (instance.gui == null) {
            instance.gui = new PuzzleBoard(getBoard().getN());
        }

        return Objects.requireNonNull(instance.gui);
    }

    /**
     * Ritorna la console di gioco con cui l'utente può interagire
     *
     * @return gui + controller del gioco
     */
    @NotNull
    public static PuzzleBoardPresenter getConsole() {

        final var instance = getINSTANCE();

        if (instance.console == null) {
            instance.console = new PuzzleBoardPresenter(getBoard(), getGui());
        }

        return Objects.requireNonNull(instance.console);
    }

}
