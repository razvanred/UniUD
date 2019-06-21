package ro.razvan.uniud.nQueensWithGui.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import queens.ChessboardView;
import ro.razvan.uniud.generics.SList;

final class Queens {

    static final SLisbot<Board> NULL_BOARD_LIST = new SList<>();

    static int numberOfSolutions(final int n) {
        return numberOfCompletions(new Board(n));
    }

    @NotNull
    static SList<Board> listOfAllSolutions(final int size) {
        return listOfAllCompletions(new Board(size));
    }

    @NotNull
    private static SList<Board> listOfAllCompletions(final Board board) {

        final var size = board.getSize();
        final var queens = board.getQueens();

        if (size == queens) {
            return NULL_BOARD_LIST.cons(board);
        }

        final var i = queens + 1;
        var solutions = NULL_BOARD_LIST;

        for (int j = 1; j <= size; j++) {
            if (!board.isUnderAttack(i, j)) {
                solutions = solutions.append(listOfAllCompletions(board.addQueen(i, j)));
            }
        }

        return solutions;
    }

    static void displaySolutions(@NotNull final ChessboardView view, @NotNull SList<Board> solutions) {
        while (!solutions.isNull()) {
            view.setQueens(solutions.car().representation());
            solutions = solutions.cdr();
        }
    }

    private static int numberOfCompletions(@NotNull final Board board) {

        if (board.getQueens() == board.getSize()) {
            return 1;
        }

        var counter = 0;
        final var i = board.getQueens() + 1;

        for (int j = 1; j <= board.getSize(); j++) {
            if (!board.isUnderAttack(i, j)) {
                counter += numberOfCompletions(board.addQueen(i, j));
            }
        }

        return counter;
    }

    /**
     * For testing purposes
     *
     * @param args not used
     */
    public static void main(@NotNull final String[] args) {
        println(numberOfSolutions(1) == 1);
        println(numberOfSolutions(2) == 0);
        println(numberOfSolutions(3) == 0);
        println(numberOfSolutions(4) == 2);
        println(numberOfSolutions(5) == 10);
        println(numberOfSolutions(6) == 4);
        println(numberOfSolutions(7) == 40);
        println(numberOfSolutions(8) == 92);
        println(numberOfSolutions(9) == 352);
        println(numberOfSolutions(10) == 724);
    }

    static void println(@Nullable final Object any) {
        System.out.println(any);
    }

}
