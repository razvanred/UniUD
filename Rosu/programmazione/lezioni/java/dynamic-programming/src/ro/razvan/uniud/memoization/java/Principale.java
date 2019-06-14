package ro.razvan.uniud.memoization.java;

import org.jetbrains.annotations.NotNull;

import static ro.razvan.uniud.memoization.java.LongestCommonSubsequence.*;

public class Principale {

    private static final int UNKNOWN = 0;

    private static int fibonacciRec(final int n) {
        if (n < 2) {
            return 1;
        } else {
            return fibonacciRec(n - 1) + fibonacciRec(n - 2);
        }
    }

    private static int fibonacciMemRec(final int n) {

        final var array = new int[n + 1];

        for (int i = 0; i < array.length; i++) {
            array[i] = UNKNOWN;
        }

        return fibonacciMemRec(n, array);
    }

    private static int fibonacciMemRec(final int n, @NotNull final int[] array) {

        if (array[n] == UNKNOWN) {
            array[n] = n < 2 ? 1 : fibonacciMemRec(n - 1, array) + fibonacciMemRec(n - 2, array);
        }

        return array[n];
    }

    private static int fibonacciIter(final int n) {

        var cur = 1;
        int next = 1;
        int temp;

        var i = 0;

        while (true) {

            if (i >= n) {
                return cur;
            }

            temp = cur;
            cur = next;
            next = temp + cur;

            i++;
        }

    }

    private static int manhattanRec(final int i, final int j) {
        if (i == 0 || j == 0) {
            return 1;
        } else {
            return manhattanRec(i, j - 1) + manhattanRec(i - 1, j);
        }
    }

    private static int manhattanRecMem(final int i, final int j) {
        final var matrix = new int[i + 1][j + 1];

        return manhattanRecMem(i, j, matrix);
    }

    private static int manhattanRecMem(final int i, final int j, final int[][] matrix) {

        if (matrix[i][j] == UNKNOWN) {
            if (i == 0 || j == 0) {
                matrix[i][j] = 1;
            } else {
                matrix[i][j] = manhattanRecMem(i, j - 1, matrix) + manhattanRecMem(i - 1, j, matrix);
            }
        }

        return matrix[i][j];
    }

    private static int manhattanIter(final int i, final int j) {

        final var matrix = new int[i + 1][j + 1];

        for (int m = 0; m < matrix.length; m++) {
            matrix[m][0] = 1;
        }

        for (int m = 1; m < j + 1; m++) {
            matrix[0][m] = 1;
        }

        for (int m = 1; m < matrix.length; m++) {
            for (int n = 1; n < matrix[m].length; n++) {
                matrix[m][n] = matrix[m - 1][n] + matrix[m][n - 1];
            }
        }

        return matrix[i][j];
    }

    public static void main(final String[] args) {
        println("testing fibonacci mem: " + (fibonacciMemRec(12) == fibonacciRec(12)));
        println("testing fibonacci iter: " + (fibonacciIter(12) == fibonacciIter(12)));
        println("testing manhattan mem: " + (manhattanRec(2, 3) == manhattanRecMem(2, 3)));
        println("testing manhattan iter: " + (manhattanIter(19, 12) == manhattanRecMem(19, 12)));

        final var a = "razvan";
        final var b = "razcyatn";

        println("testing llcs mem: " + (llcsRec(a, b) == llcsMemRec(a, b)));
        println("testing llcs bottom up: " + (llcsIter(a, b) == llcsMemRec(a, b)));
        println("testing lcs: " + (lcs(a, b).length() == llcsIter(a, b)));
    }

    private static void println(final Object obj) {
        System.out.println(obj);
    }

}
