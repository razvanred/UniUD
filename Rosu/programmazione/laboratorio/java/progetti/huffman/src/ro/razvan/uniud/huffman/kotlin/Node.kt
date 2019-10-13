package ro.razvan.uniud.huffman.kotlin

class Node : Comparable<Node> {

    private val ch: Char
    val weight: Int

    val left: Node?
    val right: Node?

    constructor(ch: Char, weight: Int = 0) {
        this.ch = ch
        this.weight = weight
        left = null
        right = null
    }

    constructor(left: Node, right: Node) {
        this.left = left
        this.right = right
        ch = 0.toChar()
        weight = left.weight + right.weight
    }

    /**
     * Ritorna se il nodo è una foglia dell'albero di Huffman
     */
    val isLeaf: Boolean
        get() = left == null

    val character: Char
        get() = ch

    override fun compareTo(other: Node): Int {
        return when {
            other.weight == weight -> 0
            other.weight > weight -> -1
            else -> 1
        }
    }

    /**
     * @ e \ sono caratteri speciali a cui va messo un \ in testa per poter essere riconosciuti
     * @return la stringa contenente il peso del carattere
     */
    override fun toString(): String = if (isLeaf) {

        if (ch == '@' || character == '\\') {
            "\\$character:$weight"
        } else {
            "$character:$weight"
        }

    } else {
        "@:$weight"
    }

}