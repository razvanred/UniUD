package ro.razvan.uniud.nQueensWithGui.java;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.generics.SList;

final class Board {

    private final int size;
    private final int queens;

    @NotNull
    private final SList<Integer> rows;

    @NotNull
    private final SList<Integer> columns;

    @NotNull
    private final SList<Integer> d1;

    @NotNull
    private final SList<Integer> d2;

    private Board(final int size,
                  final int queens,
                  @NotNull final SList<Integer> rows,
                  @NotNull final SList<Integer> columns,
                  @NotNull final SList<Integer> d1,
                  @NotNull final SList<Integer> d2) {
        this.size = size;
        this.queens = queens;
        this.rows = rows;
        this.columns = columns;
        this.d1 = d1;
        this.d2 = d2;
    }

    Board(final int size) {
        this.size = size;
        queens = 0;
        rows = new SList<>();
        columns = new SList<>();
        d1 = new SList<>();
        d2 = new SList<>();
    }

    int getSize() {
        return size;
    }

    int getQueens() {
        return queens;
    }

    /**
     * Verifica se un elemento è presente all'interno della lista generica
     *
     * @param list    lista in cui bisogna cercare l'elemento
     * @param element elemento da cercare nella lista
     * @param <T>     il tipo della lista e dell'elemento da esaminare
     * @return true se l'elemento si trova nella lista
     */
    private static <T> boolean contains(SList<T> list, final T element) {

        while (!list.isNull()) {
            if (list.car() == element) {
                return true;
            }

            list = list.cdr();
        }

        return false;
    }

    /**
     * Verifica se la posizione (i, j) è sotto attacco
     *
     * @param i riga
     * @param j colonna
     * @return true se la posizione è sotto attacco
     */
    final boolean isUnderAttack(final int i, final int j) {
        return contains(rows, i) ||
                contains(columns, j) ||
                contains(d1, i - j) ||
                contains(d2, i + j);
    }

    /**
     * @return Codifica testuale della situazione
     */
    final String arrangement() {

        final var builder = new StringBuilder("< ")
                .append(size).append(", ")
                .append(queens).append(", ")
                .append(rows).append(", ")
                .append(columns).append(", ")
                .append(d1).append(", ")
                .append(d2).append(", ")
                .append(" \" ");

        var curColumns = columns;
        var curRows = rows;

        while (!curColumns.isNull()) {

            builder.append((char) (curColumns.car() + 96))
                    .append(curRows.car())
                    .append(" ");

            curColumns = curColumns.cdr();
            curRows = curRows.cdr();
        }

        return builder.append(" \" >").toString();
    }

    final String representation() {

        final var builder = new StringBuilder();
        var curColumns = columns;
        var curRows = rows;

        while (!curColumns.isNull()) {

            builder.append((char) (curColumns.car() + 96))
                    .append(curRows.car())
                    .append(" ");

            curColumns = curColumns.cdr();
            curRows = curRows.cdr();
        }

        return builder.toString();
    }

    /**
     * Aggiunge una nuova regina alla scacchiera nella posizione (i, j)
     *
     * @param i riga
     * @param j colonna
     * @return nuova scacchiera con una nuova regina aggiunta
     */
    @NotNull
    final Board addQueen(final int i, final int j) {
        return new Board(size,
                queens + 1,
                rows.cons(i),
                columns.cons(j),
                d1.cons(i - j),
                d2.cons(i + j));
    }

}
