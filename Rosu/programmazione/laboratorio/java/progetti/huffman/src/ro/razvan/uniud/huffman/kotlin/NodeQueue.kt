package ro.razvan.uniud.huffman.kotlin

import ro.razvan.uniud.huffman.kotlin.util.deepToString

class NodeQueue {

    private var queue = arrayOfNulls<Node>(0)

    /**
     * Dimensione della queue
     */
    val size: Int
        get() = queue.size

    /**
     * Aggiunge l'elemento alla queue mantenendo l'array ordinato
     *
     * @param n nuovo nodo da aggiungere alla queue
     */
    fun add(n: Node) {

        var enough = false
        val queue = arrayOfNulls<Node>(size + 1)

        var j = 0

        for (i in 0 until this.queue.size) {

            if (!enough && this.queue[i]!! > n) {
                enough = true
                queue[j++] = n
            }

            queue[j++] = this.queue[i]

        }

        if (!enough || size == 0) {
            queue[size] = n
        }

        this.queue = queue

    }

    /**
     * Rimuove dall'array l'elemento di minor peso e lo resituisce
     *
     * @return nodo di minor peso presente nell'array
     */
    fun poll(): Node? {

        val element = queue.firstOrNull() ?: return null

        queue = queue.copyOfRange(1, size)

        return element
    }

    override fun toString(): String {
        return queue.deepToString()
    }


}