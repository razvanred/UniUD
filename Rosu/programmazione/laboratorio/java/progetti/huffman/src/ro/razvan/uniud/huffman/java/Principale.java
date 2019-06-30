package ro.razvan.uniud.huffman.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class Principale {

    private Principale() {
    }

    static class TestParte1 {

        private TestParte1() {
        }

        /**
         * args[0]: path assoluta del file non compresso da leggere
         * args[1]: path assoluta del file contente i codici di Huffman del file non compresso
         *
         * @param args argomenti contenenti path assolute dei file
         */
        public static void main(String[] args) {
            Huffman.Encoder.huffmanCodesCharacters(args[0], args[1]);
        }

    }

    static class TestParte2 {

        private TestParte2() {
        }

        /**
         * args[0]: path assoluta del file non compresso da leggere
         * args[1]: path assoluta del file contente i codici di Huffman del file non compresso
         * args[2]: path assoluta del file random da generare
         * args[3]: path assoluta del fine contente i codici di Huffman del file random generato
         *
         * @param args argomenti contenenti path assolute dei file
         */
        public static void main(@NotNull final String[] args) {

            Huffman.Encoder.huffmanCodesCharacters(args[0], args[1]);
            Huffman.Encoder.generateRandomTextFile(args[0], args[2]);
            Huffman.Encoder.huffmanCodesCharacters(args[2], args[3]);

        }

    }

    static class TestParte3 {

        private TestParte3() {
        }

        /**
         * args[0]: path assoluta del file non compresso da verificare
         * @param args argomenti contenenti path assolute dei file
         */
        public static void main(@NotNull final String[] args) {
            final var compressedFileLength = Huffman.Encoder.getFileLength(args[1]);

            println(Huffman.Encoder.getCompressedFileLength(args[0]) + " vs " + compressedFileLength);
        }

    }

    static void println(@Nullable final Object any) {
        System.out.println(any);
    }

}
