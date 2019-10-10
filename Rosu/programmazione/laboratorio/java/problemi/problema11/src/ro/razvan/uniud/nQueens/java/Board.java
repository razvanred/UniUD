package ro.razvan.uniud.nQueens.java;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.intSList.IntSList;

class Board {

    private final int size;
    private final int queens;

    @NotNull
    private final IntSList rows;

    @NotNull
    private final IntSList columns;

    @NotNull
    private final IntSList d1;

    @NotNull
    private final IntSList d2;

    private Board(final int size,
                  final int queens,
                  @NotNull final IntSList rows,
                  @NotNull final IntSList columns,
                  @NotNull final IntSList d1,
                  @NotNull final IntSList d2) {
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
        rows = new IntSList();
        columns = new IntSList();
        d1 = new IntSList();
        d2 = new IntSList();
    }

    int getSize() {
        return size;
    }

    int getQueens() {
        return queens;
    }

    /**
     * @return codifica testuale della configurazione
     */
    @NotNull
    String arragement() {

        final var builder = new StringBuilder("< ")
                .append(size).append(", ")
                .append(queens).append(", ")
                .append(rows).append(", ")
                .append(columns).append(", ")
                .append(d1).append(", ")
                .append(d2).append(", \" ");

        var curColumns = columns;
        var curRows = rows;

        while (!curColumns.isNull()) {

            builder.append((char) curColumns.getCar() + 96)
                    .append(curRows.getCar())
                    .append(" ");

            curColumns = curColumns.getCdr();
            curRows = curRows.getCdr();
        }

        return builder.append(" \" >").toString();
    }

    /**
     * Verifica se l'elemento è presente all'interno della lista
     *
     * @param list    lista da esaminare
     * @param element elemento da cercare
     * @return true se l'elemento è presente nella lista
     */
    private static boolean contains(IntSList list, final int element) {

        while (!list.isNull()) {

            if (list.getCar() == element) {
                return true;
            }

            list = list.getCdr();
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

    final Board addQueen(final int i, final int j) {
        return new Board(size, queens + 1, rows.cons(i), columns.cons(j), d1.cons(i - j), d2.cons(i + j));
    }

}
