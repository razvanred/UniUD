package ro.razvan.uniud.compitino2014;

import java.util.stream.IntStream;

public class DistributoreAutomatico {

    enum Moneta {
        CINQUE(5);

        final int value;

        Moneta(final int value) {
            this.value = value;
        }
    }

    private final int[] monete = IntStream.generate(() -> 0).limit(Moneta.values().length).toArray();

    public void introduciMonete(final int v, final Moneta n) {
        monete[n.ordinal()] += v;
    }

}
