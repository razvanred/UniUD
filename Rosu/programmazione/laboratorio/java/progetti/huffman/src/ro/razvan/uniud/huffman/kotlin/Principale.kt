package ro.razvan.uniud.huffman.kotlin

object TestParte1 {

    /**
     * args[0]: path assoluta del file non compresso da leggere
     * args[1]: path assoluta del file contente i codici di Huffman del file non compresso
     *
     * @param args argomenti contenenti path assolute dei file
     */
    @JvmStatic
    fun main(args: Array<String>) {
        Huffman.Encoder.huffmanCodesCharacters(args[0], args[1])
    }

}

object TestParte2 {

    /**
     * args[0]: path assoluta del file non compresso da leggere
     * args[1]: path assoluta del file contente i codici di Huffman del file non compresso
     * args[2]: path assoluta del file random da generare
     * args[3]: path assoluta del fine contente i codici di Huffman del file random generato
     *
     * @param args argomenti contenenti path assolute dei file
     */
    @JvmStatic
    fun main(args: Array<String>) {
        Huffman.Encoder.huffmanCodesCharacters(args[0], args[1])
        Huffman.Encoder.generateRandomTextFile(args[0], args[2])
        Huffman.Encoder.huffmanCodesCharacters(args[2], args[3])
    }

}

object TestParte3 {

    /**
     * args[0]: path assoluta del file non compresso da verificare
     *
     * @param args argomenti contenenti path assolute dei file
     */
    @JvmStatic
    fun main(args: Array<String>) {
        println("${Huffman.Encoder.getCompressedFileLength(args[0])} vs ${Huffman.Encoder.getFileLength(args[1])}")
    }

}

object TestParte6 {

    /**
     * args[0]: path assoluta del file non compresso da verificare
     *
     * @param args argomenti contenenti path assolute dei file
     */
    @JvmStatic
    fun main(args: Array<String>) {
        val freq = Huffman.Encoder.charHistogram(args[0])

        println(Huffman.Encoder.huffmanTree(freq) === Huffman.Encoder.huffmanTreeWithNodeQueue(freq))

        val root = Huffman.Encoder.huffmanTree(freq)

        println(
            Huffman.Encoder.huffmanCodesTableIterWithNodeStack(root) contentEquals Huffman.Encoder.huffmanCodesTableIter(
                root
            )
        )
    }

}