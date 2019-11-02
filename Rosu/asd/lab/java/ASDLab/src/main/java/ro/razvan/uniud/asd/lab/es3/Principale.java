package ro.razvan.uniud.asd.lab.es3;

import java.util.Scanner;
import java.util.stream.IntStream;
import java.util.Arrays;

class Principale {
    private Principale() {
        throw new AssertionError();
    }

    public static void main(final String[] args) {
        try (final Scanner s = new Scanner(System.in)) {
            Arrays.stream(suffixes(s.nextLine()))
                    .forEach(System.out::println);
        }
    }

    private static String[] suffixes(final String str) {
        if (str == null) {
            throw new IllegalArgumentException("The String must be not null");
        }

        return IntStream.range(0, str.length())
                .mapToObj(str::substring)
                .toArray(String[]::new);
    }

}