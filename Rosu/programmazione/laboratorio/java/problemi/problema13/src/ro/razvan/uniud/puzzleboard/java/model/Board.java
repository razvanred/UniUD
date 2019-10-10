package ro.razvan.uniud.puzzleboard.java.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import ro.razvan.uniud.puzzleboard.java.Principale;
import ro.razvan.uniud.puzzleboard.java.util.Integers;
import ro.razvan.uniud.puzzleboard.java.util.Pair;

import java.util.Arrays;
import java.util.Objects;
import java.util.Random;
import java.util.stream.IntStream;

public final class Board {

    private final int n;

    @NotNull
    private final int[] arrangement;

    private Board(final int n, @NotNull final int[] arrangement) {
        this.n = n;
        this.arrangement = arrangement;
    }

    /**
     * Verifica se le caselle sono in ordine
     *
     * @return true se le caselle sono in ordine
     */
    public boolean areTilesSorted() {

        for (int i = 1; i <= n; i++) {
            if (arrangement[i - 1] != i) {
                return false;
            }
        }

        return true;
    }

    /**
     * Verifica se l'elemento all'interno della tavoletta può essere spostato
     * <p>
     * Se la colonna è pari a 0 non ci sarà alcun elemento destro
     * Se la colonna è pari a n - 1 non ci sarà alcun elemento sinistro
     * <p>
     * Se la riga è pari a 0 non ci sarà alcun elemeno superiore
     * Se la riga è pari a n - 1 non ci sarà alcun elemento inferiore
     *
     * @param value elemento da spostare
     * @return true se l'elemento è spostabile
     */
    public boolean isMovable(final int value) {

        if (n <= 1) {
            return false;
        }

        final var position = findPosition(value);

        final var coordinates = getRowAndColumn(position);

        final int column = coordinates.getSecondNonNull();
        final int row = coordinates.getFirstNonNull();

        final var left = column == 0 ? -1 : arrangement[position - 1];
        final var right = column == n - 1 ? -1 : arrangement[position + 1];

        final var top = row == 0 ? -1 : arrangement[position - n];
        final var bottom = row == n - 1 ? -1 : arrangement[position + n];

        final var size = Math.pow(n, 2);

        return top == size || bottom == size || left == size || right == size;
    }

    /**
     * Trova la riga e la colonna di un elemento all'interno dell'array arrangements
     *
     * @param position posizione di un'elemento all'interno dell'array arrangements
     * @return coordinate del tassello (partendo da 0)
     */
    @NotNull
    public Pair<Integer, Integer> getRowAndColumn(final int position) {

        final var row = IntStream.range(0, n)
                .filter((element) -> position < (element + 1) * n)
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Invalid value given"));

        final var column = position % n;

        return new Pair<>(row, column);
    }

    /**
     * Trova la posizione di un determinato tassello all'interno dell'array arrangements
     *
     * @param value valore del tassello
     * @return posizione del tassello
     */
    public int findPosition(final int value) {
        return IntStream.range(0, arrangement.length)
                .filter((i) -> arrangement[i] == value)
                .findFirst()
                .orElse(-1);
    }

    /**
     * Verifica se è possibile muovere un determinato tassello e muove il tassello
     *
     * @param value valore del tassello da muovere
     * @return true se il tassello è stato mosso con successo
     */
    public boolean moveTile(final int value) {

        if (n == 1) {
            return false;
        }

        final var position = findPosition(value);

        final var coordinates = getRowAndColumn(position);

        final int column = coordinates.getSecondNonNull();
        final int row = coordinates.getFirstNonNull();

        final var leftPosition = position - 1;
        final var rightPosition = position + 1;
        final var topPosition = position - n;
        final var bottomPosition = position + n;

        final var left = column == 0 ? -1 : arrangement[leftPosition];
        final var right = column == n - 1 ? -1 : arrangement[rightPosition];

        final var top = row == 0 ? -1 : arrangement[topPosition];
        final var bottom = row == n - 1 ? -1 : arrangement[bottomPosition];

        final var size = n * n;
        final Pair<Integer, Integer> pair;

        if (left == size) {

            pair = new Pair<>(leftPosition, left);

        } else if (right == size) {

            pair = new Pair<>(rightPosition, right);

        } else if (top == size) {

            pair = new Pair<>(topPosition, top);

        } else if (bottom == size) {

            pair = new Pair<>(bottomPosition, bottom);

        } else {

            return false;
        }

        arrangement[pair.getFirstNonNull()] = value;
        arrangement[position] = pair.getSecondNonNull();

        return true;
    }

    /**
     * Trova la posizione del tassello vuoto all'interno dell'array arrangements
     *
     * @return posizione del tassello vuoto nell'array arrangements
     */
    public int blankSpacePosition() {
        return findPosition(n * n);
    }

    /**
     * Mescola il contenuto della tavoletta eseguendo 500 mosse legali
     */
    public void shuffle() {

        for (int i = 0; i < 500; i++) {

            final var movables = Arrays.stream(arrangement)
                    .filter(this::isMovable)
                    .toArray();

            final var random = new Random();

            moveTile(movables[random.nextInt(movables.length)]);
        }
    }

    @Override
    public String toString() {

        final var builder = new StringBuilder();
        final var digits = Integers.numberOfDigits(n * n);

        for (int i = 0; i < n * n; i += n) {
            for (int j = 0; j < n; j++) {
                builder.append(
                        arrangement[i + j] == n * n
                                ? " ".repeat(digits)
                                : String.format("%0" + digits + "d", arrangement[i + j])
                ).append("\t");
            }
            builder.append("\n\n");
        }

        return builder.toString();
    }

    public int getN() {
        return n;
    }

    @NotNull
    public int[] getArrangement() {
        return arrangement;
    }

    /**
     * Classe che permette di costruire la nuova Board
     */
    public final static class Builder {

        @Nullable
        private int[] arrangement = null;

        private int n = -1;

        /**
         * Imposta la dimensione di un lato della tavoletta quadrata
         *
         * @param n dimensione del lato del quadrato
         */
        public Board.Builder setSize(final int n) {
            this.n = n;
            return this;
        }

        /**
         * Imposta l'ordinamento delle arrangement all'interno della tavoletta
         *
         * @param tiles disposizione delle arrangement
         */
        public Builder setArrangement(@NotNull final int[] tiles) {
            this.arrangement = tiles;
            setSize((int) Math.sqrt(tiles.length));

            return this;
        }

        /**
         * Costruisce e ritorna una nuova istanza di Board
         *
         * @return istanza di Board configurata
         * @throws IllegalArgumentException se n e arrangement sono incompatibili tra loro
         * @throws IllegalArgumentException se arrangement non contiene tutti i numeri da 1 a n inclusi
         */
        @NotNull
        public Board build() throws RuntimeException {

            final var arrangement = Objects.requireNonNull(this.arrangement, "arrangement has not been initialized");

            if (Math.pow(n, 2) != arrangement.length) {
                throw new IllegalArgumentException("The given N and the given arrangement of arrangement are incompatible");
            }

            IntStream.rangeClosed(1, n)
                    .forEach(
                            (element) -> {
                                if (!contains(arrangement, element)) {
                                    throw new IllegalArgumentException("The tile " + element + " is missing");
                                }
                            }
                    );

            return new Board(n, arrangement);
        }

        /**
         * Cerca se l'elemento è presente nell'array
         *
         * @param array   array in cui l'elemento è da cercare
         * @param element elemento da cercare
         * @return true se l'elemento è presente nell'array
         */
        private static boolean contains(@NotNull final int[] array, final int element) {

            for (final int value : array) {
                if (value == element) {
                    return true;
                }
            }

            return false;
        }

    }

}
