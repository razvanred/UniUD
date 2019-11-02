package ro.razvan.uniud.asd.lab.es4;

import java.util.stream.IntStream;

class StringUtils {
    private StringUtils() {
        throw new AssertionError();
    }

    static String[] prefixes(final String s) {
        if (s == null) {
            throw new IllegalArgumentException("The string should be not null");
        }

        return IntStream.range(0, s.length())
                .mapToObj((i) -> s.substring(0, s.length() - i))
                .toArray(String[]::new);
    }
}
