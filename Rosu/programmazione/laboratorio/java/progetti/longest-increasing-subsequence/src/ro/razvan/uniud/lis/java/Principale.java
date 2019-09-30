package ro.razvan.uniud.lis.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static ro.razvan.uniud.lis.java.LongestIncresingSubsequence.*;

final class Principale {

    public static void main(@NotNull final String[] args) {

        final var array0 = new int[]{5, 4, 3, 2, 1};
        final var array1 = new int[]{47, 38, 39, 25, 44};
        final var array2 = new int[]{27, 90, 7, 29, 49, 8, 53, 1, 28, 6};
        final var array3 = new int[]{9, 46, 54, 71, 60, 47, 0, 32, 25, 61};
        final var array4 = new int[]{54, 52, 42, 33, 14, 40, 37, 61, 53, 1};

        println("--- Test Parte 1 ---");
        println(llisTopDown(array0) == llisRec(array0));
        println(llisTopDown(array1) == llisRec(array1));
        println(llisTopDown(array2) == llisRec(array2));
        println(llisTopDown(array3) == llisRec(array3));
        println(llisTopDown(array4) == llisRec(array4));

        println();

        println("--- Test Parte 2 ---");
        println("* LIS Recursive *");
        println(llisTopDown(array0) == lisRec(array0).getLength());
        println(llisTopDown(array1) == lisRec(array1).getLength());
        println(llisTopDown(array2) == lisRec(array2).getLength());
        println(llisTopDown(array3) == lisRec(array3).getLength());
        println(llisTopDown(array4) == lisRec(array4).getLength());

        println("* LIS Top Down == LIS Recursive *");
        println(lisTopDown(array0).equals(lisRec(array0)));
        println(lisTopDown(array1).equals(lisRec(array1)));
        println(lisTopDown(array2).equals(lisRec(array2)));
        println(lisTopDown(array3).equals(lisRec(array3)));
        println(lisTopDown(array4).equals(lisRec(array4)));

        println();

        println("--- Test Parte 3 ---");
        println(llisTopDownDebugInits(array0));
        println(llisTopDownDebugInits(array1));
        println(llisTopDownDebugInits(array2));
        println(llisTopDownDebugInits(array3));
        println(llisTopDownDebugInits(array4));

    }

    static void println() {
        System.out.println();
    }

    static void println(@Nullable final Object object) {
        System.out.println(object);
    }

}
