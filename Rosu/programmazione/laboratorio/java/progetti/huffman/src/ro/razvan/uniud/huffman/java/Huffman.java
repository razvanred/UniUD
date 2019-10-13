package ro.razvan.uniud.huffman.java;

import huffman_toolkit.InputTextFile;
import huffman_toolkit.OutputTextFile;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

class Huffman {

    private static final int CHARS = InputTextFile.CHARS;

    @Contract(" -> fail")
    private Huffman() {
        throw new AssertionError("No Huffman instances for you");
    }

    /**
     * Albero di Huffman "standard" per le testate giornalistiche inglesi
     *
     * @return radice dell'albero di Huffman
     */
    @NotNull
    static Node englishHuffmanTree() {

        final var averageWordLength = 5.1; // chars
        final var averageSentenceLength = 24.5; // words
        final var averageParagraphLength = 2.5; // sentences

        final var averageCharsNumber = 100_000;
        final var averageWordsNumber = averageCharsNumber / averageWordLength; // 19607.843137254902
        final var averageSentencesNumber = averageCharsNumber / (averageWordLength * averageSentenceLength); // 800.3201280512205
        final var averageParagraphsNumber = averageCharsNumber / (averageParagraphLength * averageSentenceLength * averageWordLength); // 320.1280512204882

        final var lowerCaseChars = new Node[]
                {
                        new Node('a', 8_167),
                        new Node('b', 1_492),
                        new Node('c', 2_782),
                        new Node('d', 4_253),
                        new Node('e', 12_702),
                        new Node('f', 2_228),
                        new Node('g', 2_015),
                        new Node('h', 6_094),
                        new Node('i', 6_966),
                        new Node('j', 153),
                        new Node('k', 772),
                        new Node('l', 4_025),
                        new Node('m', 2_406),
                        new Node('n', 6_749),
                        new Node('o', 7_507),
                        new Node('p', 1_929),
                        new Node('q', 95),
                        new Node('r', 5_987),
                        new Node('s', 6_327),
                        new Node('t', 9_056),
                        new Node('u', 2_758),
                        new Node('v', 978),
                        new Node('w', 2_361),
                        new Node('x', 150),
                        new Node('y', 1_974),
                        new Node('z', 74)
                };

        final var upperCaseChars = Arrays.stream(lowerCaseChars)
                .map((element) ->
                        new Node(
                                ((char) (((int) element.getCharacter()) - 32)),
                                element.getWeight() / ((int) Math.floor(averageSentenceLength) * (int) Math.floor(averageWordLength))
                        ))
                .toArray(Node[]::new);

        final var punctuationChars = new Node[]{
                new Node('.', (int) Math.floor(averageSentencesNumber)),
                new Node(',', (int) Math.floor(averageSentencesNumber) + 200)
        };

        final var spaceChar = new Node(' ', (int) Math.floor(
                averageSentencesNumber * (averageWordLength - 1)
        ));

        final var terminalChars = new Node[]{
                new Node('\n', (int) Math.floor(averageParagraphsNumber)),
                new Node('\r', (int) Math.floor(averageParagraphsNumber))
        };

        final var quotationMarksChar = new Node('"', (int) Math.floor(averageParagraphsNumber));

        final var numbersChars = IntStream.rangeClosed('0', '9')
                .mapToObj((element) -> new Node((char) element, (int) Math.floor(averageParagraphsNumber)))
                .toArray(Node[]::new);

        final var otherChars = new Node[]{ //30
                new Node('!', 1),
                new Node('$', 1),
                new Node('&', 1),
                new Node('%', 1),
                new Node('/', 1),
                new Node('(', 1),
                new Node(')', 1),
                new Node('=', 1),
                new Node('?', 1),
                new Node('^', 1),
                new Node('\\', 1),
                new Node('\'', 1),
                new Node('<', 1),
                new Node('>', 1),
                new Node('+', 1),
                new Node('-', 1),
                new Node('*', 1),
                new Node('[', 1),
                new Node(']', 1),
                new Node('{', 1),
                new Node('}', 1),
                new Node('#', 1),
                new Node('@', 1),
                new Node('\t', 1),
                new Node('|', 1),
                new Node('_', 1),
                new Node(':', 1),
                new Node(';', 1),
                new Node('~', 1),
                new Node('`', 1)
        };

        final var queue = new PriorityQueue<Node>();

        Stream.of(lowerCaseChars, upperCaseChars, punctuationChars, terminalChars, numbersChars, otherChars)
                .map(Arrays::asList)
                .forEach(queue::addAll);

        queue.add(spaceChar);
        queue.add(quotationMarksChar);

        while (queue.size() > 1) {

            final var left = Objects.requireNonNull(queue.poll());
            final var right = Objects.requireNonNull(queue.poll());
            queue.add(new Node(left, right));

        }

        return Objects.requireNonNull(queue.poll());
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

            /* Attraverso l'interfaccia Comparable questa lista è in grado di mantenere ordinati i suoi elementi */
            //final var queue = new PriorityQueue<Node>();
            final var queue = new PriorityQueue<Node>();

            /* vengono aggiunti alla coda solo i caratteri che hanno almeno un'occorrenza nel testo */
            for (int i = 0; i < freq.length; i++) {
                if (freq[i] > 0) {
                    queue.add(
                            new Node((char) i, freq[i])
                    );
                }
            }

            /* finché c'è più di un nodo nella coda... */
            while (queue.size() > 1) {

                /* ... tolgo i primi due nodi di peso minimo... */
                final var left = queue.poll();
                final var right = queue.poll();

                /* ...costruisco un sotto albero e lo inserisco in coda */
                queue.add(new Node(left, right));

            }

            /* alla fine di questa operazione rimarrà sempre e comunque un solo albero */

            return Objects.requireNonNull(queue.poll(), "the string was empty");
        }

        /**
         * Albero binario di codifica dei caratteri
         *
         * @param freq istogramma delle frequenze
         * @return la radice dell'albero di Huffman
         */
        @NotNull
        static Node huffmanTreeWithNodeQueue(@NotNull final int[] freq) {

            /* Attraverso l'interfaccia Comparable questa lista è in grado di mantenere ordinati i suoi elementi */
            final var queue = new NodeQueue();

            /* vengono aggiunti alla coda solo i caratteri che hanno almeno un'occorrenza nel testo */
            for (int i = 0; i < freq.length; i++) {
                if (freq[i] > 0) {
                    queue.add(
                            new Node((char) i, freq[i])
                    );
                }
            }

            /* finché c'è più di un nodo nella coda... */
            while (queue.size() > 1) {

                /* ... tolgo i primi due nodi di peso minimo... */
                final var left = Objects.requireNonNull(queue.poll());
                final var right = Objects.requireNonNull(queue.poll());

                /* ...costruisco un sotto albero e lo inserisco in coda */
                queue.add(new Node(left, right));

            }

            /* alla fine di questa operazione rimarrà sempre e comunque un solo albero */

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
         * Tabella di codifica dei caratteri - versione iterativa
         *
         * @param root nodo della radice dell'albero di Huffman
         * @return tabella compilata
         */
        @SuppressWarnings("Duplicates")
        @NotNull
        public static String[] huffmanCodesTableIter(@NotNull final Node root) {

            final var codes = new String[CHARS];

            final var stack = new Stack<Node>();
            stack.push(root);

            var code = "";

            do {

                final var n = Objects.requireNonNull(stack.pop());

                if (n.isLeaf()) {

                    codes[n.getCharacter()] = code;

                    final var k = code.lastIndexOf('0');

                    if (k >= 0) {
                        code = code.substring(0, k) + "1";
                    }

                } else {

                    stack.push(Objects.requireNonNull(n.getRight()));
                    stack.push(Objects.requireNonNull(n.getLeft()));

                    code += "0";

                }

            } while (!stack.isEmpty());

            return codes;
        }

        /**
         * Tabella di codifica dei caratteri - versione iterativa
         *
         * @param root nodo della radice dell'albero di Huffman
         * @return tabella compilata
         */
        @SuppressWarnings("Duplicates")
        @NotNull
        public static String[] huffmanCodesTableIterWithNodeStack(@NotNull final Node root) {

            final var codes = new String[CHARS];

            final var stack = new NodeStack();
            stack.push(root);

            var code = "";

            do {

                final var n = Objects.requireNonNull(stack.pop());

                if (n.isLeaf()) {

                    codes[n.getCharacter()] = code;

                    final var k = code.lastIndexOf('0');

                    if (k >= 0) {
                        code = code.substring(0, k) + "1";
                    }

                } else {

                    stack.push(Objects.requireNonNull(n.getRight()));
                    stack.push(Objects.requireNonNull(n.getLeft()));

                    code += "0";

                }

            } while (!stack.isEmpty());

            return codes;
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
         * Compressione con l'englishHuffmanTree
         *
         * @param inputPath  path assoluta del file da comprimere
         * @param outputPath path assoluta del file da decomprimere
         */
        static void standardCompress(@NotNull final String inputPath, @NotNull final String outputPath) {

            // Scansione I : frequenze dei carattteri

            final var root = englishHuffmanTree();

            var count = 0;
            final var codes = huffmanCodesTable(root);

            // Scansione II : codifica di Huffman

            var inputFile = new InputTextFile(inputPath);
            final var outputFile = new OutputTextFile(outputPath);

            while (inputFile.textAvailable()) {

                int c = inputFile.readChar();
                count++;
            }

            inputFile.close();
            inputFile = new InputTextFile(inputPath);

            outputFile.writeTextLine("" + count);

            for (int i = 0; i < count; i++) {
                final var c = inputFile.readChar();
                outputFile.writeCode(codes[c]);
            }

            inputFile.close();
            outputFile.close();
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
         * @param inputPath  path del file da leggere per trovare la lunghezza del file random generato
         * @param outputPath path del file random generato
         */
        static void generateRandomTextFile(@NotNull final String inputPath, @NotNull final String outputPath) {

            final var outputFile = new OutputTextFile(outputPath);

            try {

                final var length = getFileLength(inputPath);

                for (int i = 0; i < length; i++) {
                    outputFile.writeChar(((char) ((int) (Math.random() * 127))));
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
        static int getFileLength(@NotNull final String path) {
            return huffmanTree(charHistogram(path)).getWeight();
        }

        /**
         * Calcola la dimensione in byte del testo compresso
         *
         * @param inputPath path assoluta del file da leggere
         * @return dimensione in byte del testo compresso
         */
        static int getCompressedFileLength(@NotNull final String inputPath) {

            final var freq = charHistogram(inputPath);
            final var root = huffmanTree(freq);
            final var codes = huffmanCodesTable(root);

            final var bodyLength = IntStream.range(0, freq.length)
                    .mapToObj((i) -> new Pair<>(i, freq[i]))
                    .filter((pair) -> pair.getSecondNonNull() > 0)
                    .mapToInt((pair) -> {
                        final int i = pair.getFirstNonNull();
                        return pair.getSecondNonNull() + Objects.requireNonNull(codes[i]).length();
                    })
                    .sum() / 7;

            return bodyLength + Integer.toString(root.getWeight()).length() + flattenTree(root).length();
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
         * - una foglia è rappresentata dal carattere corrispondente
         * - un albero con più di un nodo è rappresentato dalla linearizzazione dei sottoalberi sinistro e destro
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

        /**
         * Decompressione con l'englishHuffmanTree
         *
         * @param inputPath  path assoluta del file da decomprimere
         * @param outputPath path assoluta del file decompresso geenrato
         */
        static void standardDecompress(@NotNull final String inputPath, @NotNull final String outputPath) {

            final var in = new InputTextFile(inputPath);

            final int count = Integer.parseInt(in.readTextLine());
            final var root = englishHuffmanTree();

            final var out = new OutputTextFile(outputPath);

            for (int i = 0; i < count; i++) {
                char c = decodeNextChar(root, in);
                out.writeChar(c);
            }

            in.close();
            out.close();
        }

    }
}
