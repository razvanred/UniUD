package ro.razvan.uniud.huffman.kotlin

object Principale {

    object Encoder {

        @JvmStatic
        fun main(args: Array<String>) {
            Huffman.Encoder.compress(args[0], args[1])
        }

    }

    object Decoder {

        @JvmStatic
        fun main(args: Array<String>) {
            Huffman.Decoder.decompress(args[0], args[1])
        }

    }

}