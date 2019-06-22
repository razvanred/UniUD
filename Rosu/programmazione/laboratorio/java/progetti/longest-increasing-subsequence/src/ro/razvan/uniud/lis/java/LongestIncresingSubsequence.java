package ro.razvan.uniud.lis.java;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.intSList.IntSList;

import java.util.Arrays;

final class LongestIncresingSubsequence {

    /**
     * Data una sequenza s di n interi positivi rappresentata da un array,
     * calcola la lunghezza della più lunga sottosequenza di s strettamente
     * crescente
     *
     * @param s sequenza di interi positivi
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    static int llisRec(@NotNull final int[] s) {
        return llisRec(s, 0, 0);
    }

    /**
     * Caso base: la coda è vuota, quindi con i = s.length
     * <p>
     * Gli elementi della sottosequenza devono essere strettamente maggiori di t
     * <p>
     * Se s[i] non è maggiore di t non può far parte della sottosequenza,
     * quindi ci si riconduce direttamente al problema per una coda più corta, partendo da i + 1.
     * <p>
     * Altrimenti, s[i] può far parte della sottosequenza più lunga.
     * Se vi fa parte, i successivi elementi (da cercare nella coda che inizia in posizione i+1)
     * devono essere maggiori di s[i].
     * Se non vi fa parte il vincolo aggiuntivo resta determinato da t, e si sceglierà l'opzione
     * più favorevole.
     *
     * @param s array di elementi
     * @param i indice di partenza della sottosequenza
     * @param t elemento che deve essere strettamente più grande di s[i]
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    private static int llisRec(@NotNull final int[] s, final int i, final int t) {

        if (i == s.length) {

            /* la sottosequenza è vuota */
            return 0;

        } else if (s[i] <= t) {

            /* l'elemento non fa parte della sottosequenza */
            return llisRec(s, i + 1, t);
        } else {

            /* l'elemento potrebbe far parte della sottosequenza più lunga, per questo si aggiunte 1 */
            return Math.max(
                    1 + llisRec(s, i + 1, s[i]),
                    llisRec(s, i + 1, t));
        }

    }

    private static final int UNKNOWN = -1;

    /**
     * Versione più efficiente del metodo llisRec
     *
     * @param s sequenza di interi positivi
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    static int llisTopDown(@NotNull final int[] s) {

        final var matrix = new int[s.length + 1][s.length + 1];

        for (int i = 0; i < matrix.length; i++) {
            matrix[i] = Arrays.stream(matrix[i]).map((element) -> UNKNOWN).toArray();
        }

        return llisTopDown(s, 0, 0, matrix);
    }

    /**
     * Versione più efficiente del metodo llisRec
     *
     * @param s      sequenza di interi positivi
     * @param i      indice di partenza della sottosequenza da esaminiare
     * @param j      indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @param matrix stato dell'elaborazione
     * @return lunghezza della più lunga sottosequenza di s strettamente crescente
     */
    private static int llisTopDown(@NotNull final int[] s, final int i, final int j, @NotNull final int[][] matrix) {

        if (matrix[i][j] == UNKNOWN) {

            if (s.length == i) {
                matrix[i][j] = 0;
            } else {

                final var t = j == 0 ? 0 : s[j - 1];

                if (s[i] <= t) {
                    matrix[i][j] = llisTopDown(s, i + 1, j, matrix);
                } else {
                    matrix[i][j] = Math.max(
                            1 + llisTopDown(s, i + 1, i + 1, matrix),
                            llisTopDown(s, i + 1, j, matrix)
                    );
                }

            }

        }

        return matrix[i][j];
    }

    /**
     * Versione del metodo llisTopDown che ritorna in un oggetto Pair la lunghezza della sottosequenza crescente
     * più lunga e il numero di inizializzazioni eseguite dal metodo sulle celle della matrice matrix
     *
     * @param s sequenza di interi positivi
     * @return oggetto Pair contente la lunghezza della sottosequenza crescente e il numero di inizializzazioni
     * eseguite dal metodo sulle celle della matrice matrix
     */
    @NotNull
    static Pair<Integer, Integer> llisTopDownDebugInits(@NotNull final int[] s) {

        final var matrix = new int[s.length + 1][s.length + 1];

        for (int i = 0; i < matrix.length; i++) {
            matrix[i] = Arrays.stream(matrix[i]).map((element) -> UNKNOWN).toArray();
        }

        return llisTopDownDebugInits(s, 0, 0, matrix);
    }

    /**
     * Versione del metodo llisTopDown che ritorna in un oggetto Pair la lunghezza della sottosequenza crescente
     * più lunga e il numero di inizializzazioni eseguite dal metodo sulle celle della matrice matrix
     *
     * @param s      sequenza di interi positivi
     * @param i      indice di partenza della sottosequenza da esaminiare
     * @param j      indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @param matrix stato dell'elaborazione
     * @return oggetto Pair contente la lunghezza della sottosequenza crescente e il numero di inizializzazioni
     * eseguite dal metodo sulle celle della matrice matrix
     */
    @NotNull
    private static Pair<Integer, Integer> llisTopDownDebugInits(
            @NotNull final int[] s,
            final int i,
            final int j,
            @NotNull final int[][] matrix
    ) {

        final int counter;

        if (matrix[i][j] == UNKNOWN) {

            if (s.length == i) {

                matrix[i][j] = 0;
                counter = 1;

            } else {

                final var t = j == 0 ? 0 : s[j - 1];

                if (s[i] <= t) {

                    final var pair = llisTopDownDebugInits(s, i + 1, j, matrix);

                    matrix[i][j] = pair.getFirstNonNull();
                    counter = pair.getSecondNonNull() + 1;

                } else {

                    final var left = llisTopDownDebugInits(s, i + 1, i + 1, matrix);
                    final var right = llisTopDownDebugInits(s, i + 1, j, matrix);

                    final var leftSize = 1 + left.getFirstNonNull();
                    final var rightSize = right.getFirstNonNull();

                    if (leftSize > rightSize) {
                        matrix[i][j] = leftSize;
                    } else {
                        matrix[i][j] = rightSize;
                    }

                    counter = left.getSecondNonNull() + right.getSecondNonNull() + 1;

                }

            }

        } else {
            counter = 0;
        }

        return new Pair<>(matrix[i][j], counter);
    }

    /**
     * Longest Increasing Sub-sequence
     *
     * @param s sequenza di interi positivi
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    @NotNull
    static IntSList lisRec(@NotNull final int[] s) {
        return lisRec(s, 0, 0);
    }

    /**
     * Longest Increasing Sub-sequence
     *
     * @param s sequenza di interi positivi
     * @param i indice di partenza della sottosequenza da esaminare
     * @param t indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    @NotNull
    private static IntSList lisRec(@NotNull final int[] s, final int i, final int t) {

        if (s.length == i) {
            return IntSList.Companion.getNULL_INT_S_LIST();
        } else if (s[i] <= t) {
            return lisRec(s, i + 1, t);
        } else {
            final var left = new IntSList(s[i], lisRec(s, i + 1, s[i]));
            final var right = lisRec(s, i + 1, t);

            return Math.max(left.getLength(), right.getLength()) == left.getLength() ? left : right;
        }

    }

    /**
     * LIS con la tecnica Top-Down applicata
     *
     * @param s sequenza di interi positivi
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    @NotNull
    static IntSList lisTopDown(@NotNull final int[] s) {

        final var matrix = new IntSList[s.length + 1][s.length + 1];

        return lisTopDown(s, 0, 0, matrix);
    }

    /**
     * LIS con la tecnica Top-Down applicata
     *
     * @param s      sequenza di interi positivi
     * @param i      indice di partenza della sottosequenza da esaminare
     * @param j      indice che permette di determinare t leggendo l'elemento in posizione j nella sequenza estesa
     * @param matrix stato dell'elaborazione
     * @return sotto-sequenza di interi positivi crescente più lunga
     */
    @NotNull
    private static IntSList lisTopDown(@NotNull final int[] s,
                                       final int i,
                                       final int j,
                                       @NotNull final IntSList[][] matrix) {
        if (matrix[i][j] == null) {

            if (s.length == i) {
                matrix[i][j] = IntSList.Companion.getNULL_INT_S_LIST();
            } else {
                final var t = j == 0 ? 0 : s[j - 1];

                if (s[i] <= t) {
                    matrix[i][j] = lisTopDown(s, i + 1, j, matrix);
                } else {
                    final var left = new IntSList(s[i], lisTopDown(s, i + 1, i + 1, matrix));
                    final var right = lisTopDown(s, i + 1, j, matrix);

                    matrix[i][j] = Math.max(left.getLength(), right.getLength()) == left.getLength() ? left : right;
                }
            }

        }

        return matrix[i][j];
    }

}
