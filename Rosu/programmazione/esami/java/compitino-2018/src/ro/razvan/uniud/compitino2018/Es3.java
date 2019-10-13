package ro.razvan.uniud.compitino2018;

import java.util.stream.IntStream;

public final class Es3 {

    private static final int UNKNWON = -1;

    public static int q(final int[] s) {

        final var n = s.length;
        final var t = new int[n];
        t[0] = s[0];

        for (int k = 1; k < n; k++) {

            var i = k - 1;

            while (i >= 0 && t[i] > s[k]) {
                t[i + 1] = t[i];
                i--;
            }

            t[i + 1] = s[k];

        }


        return qRec(s, t, n, 0, 0);
    }

    private static int qRec(final int[] s, final int[] t, final int n, final int i, final int j) {

        if (i == n || j == n) {
            return 0;
        } else if (s[i] == t[j]) {
            return 1 + qRec(s, t, n, i + 1, j + 1);
        } else {
            return Math.max(
                    qRec(s, t, n, i + 1, j),
                    qRec(s, t, n, i, j + 1)
            );
        }
    }

    public static int qTopDown(final int[] s) {

        final var n = s.length;
        final var t = new int[n];
        t[0] = s[0];

        for (int k = 1; k < n; k++) {

            var i = k - 1;

            while (i >= 0 && t[i] > s[k]) {
                t[i + 1] = t[i];
                i--;
            }

            t[i + 1] = s[k];

        }

        final var matrix = new int[n + 1][n + 1];

        for (int i = 0; i < matrix.length; i++) {
            matrix[i] = IntStream.generate(() -> UNKNWON).limit(matrix.length).toArray();
        }

        return qRecTopDown(s, t, n, 0, 0, matrix);
    }

    private static int qRecTopDown(final int[] s, final int[] t, final int n, final int i, final int j, final int[][] matrix) {

        if (matrix[i][j] == UNKNWON) {

            if (i == n || j == n) {
                matrix[i][j] = 0;
            } else if (s[i] == t[j]) {
                matrix[i][j] = 1 + qRecTopDown(s, t, n, i + 1, j + 1, matrix);
            } else {
                matrix[i][j] = Math.max(
                        qRecTopDown(s, t, n, i + 1, j, matrix),
                        qRecTopDown(s, t, n, i, j + 1, matrix)
                );
            }
        }

        return matrix[i][j];
    }


    public static void main(String[] args) {
        final var array0 = new int[]{1,2,3,4,5};
        final var array1 = new int[]{5,4,3,2,1};
        final var array2 = new int[]{113, 12, 11, 1423, 2};

        println(qTopDown(array0) == q(array0));
        println(qTopDown(array1) == q(array1));
        println(qTopDown(array2) == q(array2));
    }

    static void println(final Object any){
        System.out.println(any);
    }

}
