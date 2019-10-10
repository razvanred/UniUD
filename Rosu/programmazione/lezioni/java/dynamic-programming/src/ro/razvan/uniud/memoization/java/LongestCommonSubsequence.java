package ro.razvan.uniud.memoization.java;

@SuppressWarnings("Duplicates")
class LongestCommonSubsequence {

    private static final int UNKNOWN = -1;

    static int llcsRec(final String u, final String v) {
        if (u.isEmpty() || v.isEmpty()) {
            return 0;
        } else if (u.charAt(0) == v.charAt(0)) {
            return 1 + llcsRec(u.substring(1), v.substring(1));
        } else {
            return Math.max(
                    llcsRec(u.substring(1), v),
                    llcsRec(u, v.substring(1))
            );
        }
    }

    static int llcsMemRec(final String u, final String v) {
        final var matrix = new int[u.length() + 1][v.length() + 1];

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[i].length; j++) {
                matrix[i][j] = UNKNOWN;
            }
        }

        return llcsMemRec(u, v, matrix);
    }

    private static int llcsMemRec(final String u, final String v, final int[][] matrix) {

        final var i = u.length();
        final var j = v.length();

        if (matrix[i][j] == UNKNOWN) {

            if (u.isEmpty() || v.isEmpty()) {
                matrix[i][j] = 0;
            } else if (u.charAt(0) == v.charAt(0)) {
                matrix[i][j] = llcsMemRec(u.substring(1), v.substring(1)) + 1;
            } else {
                matrix[i][j] = Math.max(
                        llcsMemRec(u.substring(1), v, matrix),
                        llcsMemRec(u, v.substring(1), matrix)
                );
            }

        }

        return matrix[i][j];
    }

    static int llcsIter(final String u, final String v) {
        final var m = u.length();
        final var n = v.length();
        final var matrix = new int[m + 1][n + 1];

        for (int i = 0; i < matrix.length; i++) {
            matrix[i][0] = 0;
        }

        for (int i = 1; i <= n; i++) {
            matrix[0][i] = 0;
        }

        for (int i = 1; i < matrix.length; i++) {
            for (int j = 1; j < matrix[i].length; j++) {
                if (u.charAt(m - i) == v.charAt(n - j)) {
                    matrix[i][j] = 1 + matrix[i - 1][j - 1];
                } else {
                    matrix[i][j] = Math.max(matrix[i][j - 1], matrix[i - 1][j]);
                }
            }
        }

        return matrix[m][n];
    }


    static String lcs(final String u, final String v) {
        final var m = u.length();
        final var n = v.length();
        final var matrix = new int[m + 1][n + 1];

        for (int i = 0; i < matrix.length; i++) {
            matrix[i][0] = 0;
        }

        for (int i = 1; i <= n; i++) {
            matrix[0][i] = 0;
        }

        for (int i = 1; i < matrix.length; i++) {
            for (int j = 1; j < matrix[i].length; j++) {
                if (u.charAt(m - i) == v.charAt(n - j)) {
                    matrix[i][j] = 1 + matrix[i - 1][j - 1];
                } else {
                    matrix[i][j] = Math.max(
                            matrix[i - 1][j],
                            matrix[i][j - 1]
                    );
                }
            }
        }

        final var builder = new StringBuilder();
        var i = m;
        var j = n;

        while (matrix[i][j] != 0) {
            if (u.charAt(m - i) == v.charAt(n - j)) {
                builder.append(u.charAt(m - i));
                i--;
                j--;
            } else if (matrix[i - 1][j] < matrix[i][j - 1]) {
                j--;
            } else {
                i--;
            }
        }

        return builder.toString();
    }

}
