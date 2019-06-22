package ro.razvan.uniud.huffman.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class Principale {

    static void println(@Nullable Object any) {
        System.out.println(any);
    }

    static class Encoder {

        public static void main(@NotNull final String[] args) {
            Huffman.Encoder.compress(args[0], args[1]);
        }

    }

    static class Decoder {

        public static void main(@NotNull final String[] args) {
            Huffman.Decoder.decompress(args[0], args[1]);
        }

    }

}
