package ro.razvan.uniud.huffman.kotlin

import huffman_toolkit.InputTextFile
import huffman_toolkit.OutputTextFile
import java.util.*

object Huffman {

    private const val CHARS = InputTextFile.CHARS

    object Encoder {

        /**
         * Istogramma di frequenze dei caratteri
         *
         * @param path il percorso assoluto del file da elaborare
         * @return array contente la frequenza di ciascun carattere
         */
        fun charHistogram(path: String): IntArray {

            val freq = IntArray(CHARS) { 0 }
            val inputFile = InputTextFile(path)

            while (inputFile.textAvailable()) {
                freq[inputFile.readChar().toInt()]++
            }

            inputFile.close()

            return freq
        }

        /**
         * Albero di Huffman, costruito in base all'istogramma di frequenza dei caratteri
         *
         * @param freq istogramma di frequenza dei caratteri
         * @return la radice dell'albero generato
         * @throws IllegalArgumentException se l'istogramma non presenta caratteri
         */
        fun huffmanTree(freq: IntArray): Node {

            val queue = PriorityQueue<Node>()

            freq.mapIndexed { index, element -> Pair(index, element) }
                .filter { it.second > 0 }
                .forEach { queue.add(Node(it.first.toChar(), it.second)) }

            while (queue.size > 1) {
                val left = queue.poll()
                val right = queue.poll()
                queue.add(Node(left, right))
            }

            return queue.poll() ?: throw IllegalArgumentException("the string was empty")
        }

        /**
         * Tabella di codifica dei caratteri
         *
         * @param root nodo radice dell'albero di Huffman
         */
        fun huffmanCodesTable(root: Node): Array<String?> {

            val array = arrayOfNulls<String>(CHARS)

            fillTable(root, "", array)

            return array
        }

        /**
         * Compilazione della tabella tramite visita ricorsiva dell'albero di Huffman
         *
         */
        private fun fillTable(node: Node, code: String, codes: Array<String?>) {
            if (node.isLeaf) {
                codes[node.character.toInt()] = code
            } else {
                fillTable(node.left!!, "${code}0", codes)
                fillTable(node.right!!, "${code}1", codes)
            }
        }

        /**
         * Codifica lineare dell'albero di Huffman tramite ricorsione
         *
         * @param root la radice dell'albero di Huffman
         * @return rappresentazione testuale dell'albero di Huffman
         */
        fun flattenTree(root: Node): String = if (root.isLeaf) {
            if (root.character == '@' || root.character == '\\') {
                "\\${root.character}"
            } else {
                root.character.toString()
            }
        } else {
            "@${flattenTree(root.left!!)}${flattenTree(root.right!!)}"
        }

        /**
         * Compressione di un file
         *
         * @param inputPath path assoluta del file da comprimere
         * @param outputPath path assoluta del file compresso da salvare
         */
        fun compress(inputPath: String, outputPath: String) {

            val histogram = charHistogram(inputPath)
            val root = huffmanTree(histogram)
            val codes = huffmanCodesTable(root)
            val count = root.weight

            val inputFile = InputTextFile(inputPath)
            val outputFile = OutputTextFile(outputPath)

            outputFile.writeTextLine(count.toString())
            outputFile.writeTextLine(flattenTree(root))

            for (i in 0 until count) {
                outputFile.writeCode(codes[inputFile.readChar().toInt()])
            }

            inputFile.close()
            outputFile.close()

        }

    }

    object Decoder {

        /**
         * Ricostruzione dell'albero di Huffman dalla sua codifica lineare
         *
         * @param inputFile file contenente l'albero da ripristinare
         */
        fun restoreTree(inputFile: InputTextFile): Node {

            val c = inputFile.readChar()

            if (c == '@') {
                val left = restoreTree(inputFile)
                val right = restoreTree(inputFile)

                return Node(left, right)
            }

            if (c == '\\') {
                return Node(inputFile.readChar())
            }

            return Node(c)
        }

        /**
         * Decodifica del carattere successivo
         *
         * @param root radice dell'albero di Huffman
         * @param inputFile documento compresso
         */
        private fun decodeNextChar(root: Node, inputFile: InputTextFile): Char {

            var n = root

            do {

                n = (if (inputFile.readBit() == 0) root.left else root.right)!!

            } while (!n.isLeaf)

            return n.character
        }

        /**
         * Decompressione
         *
         * @param inputPath path assoluta del file compresso
         * @param outputPath path assoluta del file
         */
        fun decompress(inputPath: String, outputPath: String) {

            val inputFile = InputTextFile(inputPath)
            val outputFile = OutputTextFile(outputPath)

            try {

                val weight = inputFile.readTextLine().toInt()
                val root = restoreTree(inputFile)

                inputFile.readTextLine()

                for (i in 0 until weight) {
                    outputFile.writeChar(decodeNextChar(root, inputFile))
                }

            } finally {

                inputFile.close()
                outputFile.close()

            }

        }

    }

}