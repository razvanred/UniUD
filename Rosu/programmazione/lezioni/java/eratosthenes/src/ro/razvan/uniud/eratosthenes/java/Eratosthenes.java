package ro.razvan.uniud.eratosthenes.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

final class Eratosthenes {

    /**
     * Calcola tutti i numeri primo fino a n
     * Pre: n > 1
     *
     * @param n intero maggiore di 1
     * @return lista di interi primi minori o uguali a n
     */
    @NotNull
    static List<Integer> primesUpTo(final int n) {

        final var primes = new ArrayList<Integer>();

        final Boolean[] sieve = IntStream.range(0, n + 1)
                .mapToObj((element) -> element >= 2)
                .toArray(Boolean[]::new);

        for (int p = 2; p < sieve.length; p++) {

            if (sieve[p]) {

                primes.add(p);

                for (int i = p * 2; i < sieve.length; i += p) {
                    sieve[i] = false;
                }

            }

        }

        return primes;
    }

    public static void main(@NotNull final String[] args) {
        println(primesUpTo(Integer.parseInt(args[0])));
    }

    static void println(@Nullable final Object any) {
        System.out.println(any);
    }

}
