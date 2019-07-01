package ro.razvan.uniud.huffman.java.utils;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ArrayUtils {

    private ArrayUtils(){
    }

    /**
     * Metodo ispirato ad Arrays.deepToString()
     *
     * @param array array da scorrere per stampare il suo contenuto
     * @param <T>   tipo degli elementi dell'array
     * @return codifica testuale dell'array
     */
    @NotNull
    public static <T> String deepToString(@NotNull final T[] array) {

        final var builder = new StringBuilder("[");

        for (var i = 0; i < array.length; i++) {
            builder.append(array[i]);
            if (i < array.length - 1) {
                builder.append(", ");
            }
        }

        return builder.append("]").toString();
    }

    /**
     * Prende il primo valore dell'array, resituisce null se l'array è vuoto o la cella è null
     *
     * @param array array da cui prendere il primo elemento
     * @param <T> tipo degli elementi dell'array
     * @return primo elemento dell'array
     */
    @Nullable
    public static <T> T getFirstOrNull(@NotNull final T[] array) {

        if(array.length == 0){
            return null;
        }

        return array[0];
    }

}
