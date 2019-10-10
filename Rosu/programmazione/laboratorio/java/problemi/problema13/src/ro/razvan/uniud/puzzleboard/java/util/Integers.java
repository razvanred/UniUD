package ro.razvan.uniud.puzzleboard.java.util;

public final class Integers {

    private Integers() { }

    /**
     * Trova il numero di cifre necessarie per scrivere n
     *
     * @param n numero da elaborare
     * @return numero di cifre
     */
    public static int numberOfDigits(final int n) {
        return (int) Math.log10(n) + 1;
    }

}
