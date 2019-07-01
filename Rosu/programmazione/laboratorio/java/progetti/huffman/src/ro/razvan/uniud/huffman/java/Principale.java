package ro.razvan.uniud.huffman.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;

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
        public static void main(@NotNull final String[] args) {
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
         *
         * @param args argomenti contenenti path assolute dei file
         */
        public static void main(@NotNull final String[] args) {
            final var compressedFileLength = Huffman.Encoder.getFileLength(args[1]);

            println(Huffman.Encoder.getCompressedFileLength(args[0]) + " vs " + compressedFileLength);
        }

    }

    static class TestParte6 {

        private TestParte6() {
        }

        /**
         * args[0]: path assoluta del file non compresso da verificare
         *
         * @param args argomenti contenenti path assolute dei file
         */
        public static void main(@NotNull final String[] args) {

            final var freq = Huffman.Encoder.charHistogram(args[0]);

            println(Huffman.Encoder.huffmanTree(freq) == Huffman.Encoder.huffmanTreeWithNodeQueue(freq));

            final var root = Huffman.Encoder.huffmanTree(freq);

            println(Arrays.compare(Huffman.Encoder.huffmanCodesTableIter(root), Huffman.Encoder.huffmanCodesTableIterWithNodeStack(root)) == 0);
        }

    }

    static class TestParte7 {

        private TestParte7(){
        }

        public static void main(@NotNull final String[] args) {
            Huffman.englishHuffmanTree();
        }

    }

    static void println(@Nullable final Object any) {
        System.out.println(any);
    }

}
