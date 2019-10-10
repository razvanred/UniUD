package ro.razvan.uniud.stringSList.java;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.stringSList.kotlin.StringSList;

public class BtrNotation {

    private static final char PLUS_CHARACTER = '+';
    private static final char MINUS_CHARACTER = '-';
    private static final char ZERO_CHARACTER = '.';

    private BtrNotation() {
    }

    @NotNull
    static String btrSucc(@NotNull final String btr) {
        final var n = btr.length();
        final var lsb = btr.charAt(n - 1);

        final String result;

        if (n == 1) {
            switch (lsb) {
                case PLUS_CHARACTER:
                    result = Character.toString(PLUS_CHARACTER) + MINUS_CHARACTER;
                    break;
                case MINUS_CHARACTER:
                    result = Character.toString(ZERO_CHARACTER);
                    break;
                default:
                    result = Character.toString(PLUS_CHARACTER);
            }
        } else {
            final var pre = btr.substring(0, n - 1);

            if (lsb == PLUS_CHARACTER) {
                result = btrSucc(pre) + MINUS_CHARACTER;
            } else {
                result = pre + Character.toString((lsb == MINUS_CHARACTER) ?
                        ZERO_CHARACTER
                        :
                        PLUS_CHARACTER);

            }
        }

        return result;
    }

    @NotNull
    public static StringSList btrSucc(@NotNull final String btr, final int n) {
        var list = new StringSList(btr);
        var i = 0;

        while (++i < n) {
            list = list.cons(btrSucc(list.listRef(0)));
        }

        return list.reverse();
    }

}
