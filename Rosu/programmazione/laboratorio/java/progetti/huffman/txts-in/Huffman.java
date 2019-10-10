package ro.razvan.uniud.huffman.java;

import huffman_toolkit.InputTextFile;
import huffman_toolkit.OutputTextFile;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.stream.IntStream;

class Huffman {

    private static final int CHARS = InputTextFile.CHARS;

    @Contract(" -> fail")
    private Huffman() {
        throw new AssertionError("No Huffman instances for you");
    }

    static class Encoder {

        @Contract(" -> fail")
        private Encoder() {
            throw new AssertionError("No Encoder instances for you");
        }

        /**
         * Istogramma della frequenza dei caratteri
         *
         * @param path la directory del file da comprimere
         * @return array contentente il numero di ripetizioni di ciascuna lettera presente all'interno del file
         */
        @NotNull
        static int[] charHistogram(@NotNull final String path) {

            final var in = new InputTextFile(path);
            try {

                final var freq = IntStream.generate(() -> 0).limit(CHARS).toArray();

                while (in.textAvailable()) {
                    final var c = in.readChar();
                    System.out.println(c);
                    freq[c]++;
                }

                return freq;

            } finally {

                in.close();
            }
        }

        /**
         * Albero binario di codifica dei caratteri
         *
         * @param freq istogramma delle frequenze
         * @return la radice dell'albero di Huffman
         */
        @NotNull
        static Node huffmanTree(@NotNull final int[] freq) {

            /* Attraverso l'interfaccia Comparable questa lista ? in grado di mantenere ordinati i suoi elementi */
            final var queue = new PriorityQueue<Node>();

            /* vengono aggiunti alla coda solo i caratteri che hanno almeno un'occorrenza nel testo */
            for (int i = 0; i < freq.length; i++) {
                if (freq[i] > 0) {
                    queue.add(
                            new Node((char) i, freq[i])
                    );
                }
            }

            /* finch? c'? pi? di un nodo nella coda... */
            while (queue.size() > 1) {

                /* ... tolgo i primi due nodi di peso minimo... */
                final var left = queue.poll();
                final var right = queue.poll();

                /* ...costruisco un sotto albero e lo inserisco in coda */
                queue.add(new Node(left, right));

            }

            /* alla fine di questa operazione rimarr? sempre e comunque un solo albero */

            return Objects.requireNonNull(queue.poll(), "the string was empty");
        }

        /**
         * Tabella di codifica dei caratteri
         *
         * @param root nodo della radice dell'albero di Huffman
         * @return tabella compilata
         */
        @NotNull
        static String[] huffmanCodesTable(@NotNull final Node root) {

            final var codes = new String[CHARS];

            fillTable(root, "", codes);

            return codes;
        }

        /**
         * Compilazione della tabella tramite visita ricorsiva dell'albero di Huffman
         *
         * @param n     nodo visitato
         * @param code  codice binario del percorso della radice al nodo n
         * @param codes variabile di stato per registrare i codici dei caratteri
         */
        private static void fillTable(@NotNull final Node n, @NotNull final String code, @NotNull final String[] codes) {
            if (n.isLeaf()) {
                codes[n.getCharacter()] = code;
            } else {
                fillTable(Objects.requireNonNull(n.getLeft()), code + "0", codes);
                fillTable(Objects.requireNonNull(n.getRight()), code + "1", codes);
            }
        }

        /**
         * Codifica lineare dell'albero di Huffman tramite ricorsione
         *
         * @param n nodo radice dell'albero di Huffman
         * @return la stringa che rappresenta il contenuto dell'albero di Huffman
         */
        @NotNull
        static String flattenTree(@NotNull final Node n) {

            if (n.isLeaf()) {
                final var c = n.getCharacter();
                if (c == '\\' || c == '@') {
                    return "\\" + c;
                } else {
                    return Character.toString(c);
                }
            } else {
                return "@"
                        + flattenTree(Objects.requireNonNull(n.getLeft()))
                        + flattenTree(Objects.requireNonNull(n.getRight()));
            }

        }

        /**
         * Compressione di un documento di testo
         *
         * @param inputPath  la path assoluta del file da elaborare
         * @param outputPath la path assoluta della destinazione del file di output
         */
        static void compress(@NotNull final String inputPath, @NotNull final String outputPath) {

            /*
            Scansione I: frequenza dei caratteri
             */
            final var freq = charHistogram(inputPath);
            final var root = huffmanTree(freq);
            final var count = root.getWeight();
            final var codes = huffmanCodesTable(root);

            /*
            Scansione II: codifica di Huffman
             */
            final var in = new InputTextFile(inputPath);
            final var out = new OutputTextFile(outputPath);

            /* Intestazione del documento compresso */
            out.writeTextLine(Integer.toString(count));
            out.writeTextLine(flattenTree(root));

            /* Scansione: codifica dei caratteri */
            for (int i = 0; i < count; i++) {
                final char c = in.readChar();
                out.writeCode(codes[c]);
            }

            /* Chiusura dei buffer dei file */
            in.close();
            out.close();

        }

        /**
         * Analizza il file di input per scrivere su file di output la tabella che contiene i seguenti elementi:
         * <ol>
         * <li>Codice ASCII (0-127)</li>
         * <li>Simbolo del carattere corrispondente</li>
         * <li>Numero di occorrenze nel file di input</li>
         * <li>Codice di Huffman</li>
         * <li>Lunghezza del codice di Huffman</li>
         * </ol>
         * <p>
         * Gestire inoltre i caratteri speciali nuova-linea, capo-linea e tabulazione
         *
         * @param inputPath  file da leggere
         * @param outputPath file di output
         */
        static void huffmanCodesCharacters(@NotNull final String inputPath, @NotNull final String outputPath) {

            final var freq = charHistogram(inputPath);
            final var root = huffmanTree(freq);
            final var codes = huffmanCodesTable(root);

            final var outputFile = new OutputTextFile(outputPath);

            try {

                IntStream.range(0, freq.length)
                        .mapToObj((i) -> new Pair<>(i, freq[i]))
                        .filter((pair) -> pair.getSecondNonNull() > 0)
                        .forEach((pair) -> {

                            final int i = pair.getFirstNonNull();

                            final var builder = new StringBuilder(String.format("%03d", i))
                                    .append(" ; ");

                            switch (i) {
                                case '\n':
                                    builder.append("\\n");
                                    break;
                                case '\r':
                                    builder.append("\\r");
                                    break;
                                case '\t':
                                    builder.append("\\t");
                                    break;
                                default:
                                    builder.append((char) i);
                            }

                            final var code = Objects.requireNonNull(codes[i]);

                            outputFile.writeTextLine(
                                    builder.append(" ; ")
                                            .append(pair.getSecondNonNull())
                                            .append(" ; ")
                                            .append(code)
                                            .append(" ; ")
                                            .append(code.length())
                                            .toString()
                            );

                        });

            } finally {
                outputFile.close();
            }

        }

        /**
         * Genera un file di testo random composto da caratteri i cui codici ASCII sono prodotti in modo casuale,
         * con distribuzione uniforme nell'intervallo 0 - 127
         *
         * @param inputPath
         * @param outputPath
         */
        static void generateRandomTextFile(@NotNull final String inputPath, @NotNull final String outputPath) {

            final var outputFile = new OutputTextFile(inputPath);
            final var length = getFileLength(inputPath);

            try {

                for (int i = 0; i < length; i++) {
                    outputFile.writeChar(((char) (int) (Math.random() * 127)));
                }

            } finally {

                outputFile.close();

            }

        }

        /**
         * Get file length
         *
         * @param path path of the file to elaborate
         * @return length of the file
         */
        private static int getFileLength(@NotNull final String path) {
            return huffmanTree(charHistogram(path)).getWeight();
        }

    }

    static class Decoder {

        @Contract(" -> fail")
        private Decoder() {
            throw new AssertionError("No Decoder instances for you");
        }

        /**
         * Ricostruzione dell'albero di Huffman della sua codifica lineare
         * Struttura:
         * - una foglia ? rappresentata dal carattere corrispondente
         * - un albero con pi? di un nodo ? rappresentato dalla linearizzazione dei sottoalberi sinistro e destro
         *
         * @param in documento compresso
         * @return radice dell'albero di Huffman
         */
        @NotNull
        static Node restoreTree(@NotNull final InputTextFile in) {

            final var c = in.readChar();

            if (c == '@') {
                final var left = restoreTree(in);
                final var right = restoreTree(in);

                return new Node(left, right);

            }

            if (c == '\\') {
                return new Node(in.readChar(), 0);
            }

            return new Node(c, 0); // foglia
        }

        /**
         * Decodifica del carattere successivo
         *
         * @param root nodo radice dell'albero di Huffman
         * @param in   documento compresso
         * @return carattere del documento decodificato
         */
        private static char decodeNextChar(@NotNull final Node root, @NotNull final InputTextFile in) {

            var n = root;

            do {

                /*
                bit 0: figlio sinistro
                bit 1: figlio destro
                */

                n = in.readBit() == 0 ? n.getLeft() : n.getRight();

            } while (!Objects.requireNonNull(n).isLeaf());

            return n.getCharacter();
        }

        /**
         * Decompressione
         *
         * @param inputPath  path assoluta del documento compresso
         * @param outputPath path assouluta del documento ripristinato
         */
        static void decompress(@NotNull final String inputPath, @NotNull final String outputPath) {

            final var in = new InputTextFile(inputPath);
            final var out = new OutputTextFile(outputPath);

            try {

                /* Decodifica dell'intestazione */
                final var count = Integer.parseInt(in.readTextLine());
                final var root = restoreTree(in);

                /* Per andare a capo */
                in.readTextLine();

                for (int i = 0; i < count; i++) {
                    out.writeChar(decodeNextChar(root, in));
                }

            } finally {

                in.close();
                out.close();

            }
        }

    }

}
