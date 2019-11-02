package ro.razvan.uniud.asd.lab.es5;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

class IntArrayUtils {
    private IntArrayUtils() {
        throw new AssertionError();
    }

    private static final Pattern INT_ARRAY_PATTERN = Pattern.compile("\\d+");

    public static int[] valueOf(final String str) {
        final List<Integer> l = new ArrayList<>();
        final Matcher m = INT_ARRAY_PATTERN.matcher(str);
        while (m.find()) {
            l.add(Integer.valueOf(m.group()));
        }

        return valueOf(l);
    }

    public static int[] valueOf(final List<Integer> list) {
        if (list == null) {
            throw new IllegalArgumentException("The Integer array must be not null");
        }

        final int[] array = new int[list.size()];
        IntStream.range(0, array.length)
                .forEach(i -> array[i] = list.get(i));
        return array;
    }

    static int binarySearch(final int[] orderedArray, final int element) {
        if (orderedArray == null) {
            throw new IllegalArgumentException("The array should be not null");
        }

        return binarySearch(orderedArray, element, 0, orderedArray.length);
    }

    private static int binarySearch(
            final int[] orderedArray,
            final int element,
            final int startInclusive,
            final int endExclusive
    ) {
        if (startInclusive >= endExclusive) {
            return -1;
        }

        final int x = Math.floorDiv(startInclusive + endExclusive, 2);

        return orderedArray[x] == element ? x
                : orderedArray[x] < element ? binarySearch(orderedArray, element, x + 1, endExclusive)
                : binarySearch(orderedArray, element, startInclusive, x);
    }
}
