package ro.razvan.uniud.huffman.kotlin

class NodeStack {

    private var stack = arrayOfNulls<Node>(0)

    val size: Int
        get() = stack.size

    fun push(n: Node) {

        val stack = arrayOfNulls<Node>(size + 1)

        stack[0] = n

        System.arraycopy(this.stack, 0, stack, 1, size)

        this.stack = stack
    }

    val peek: Node?
        get() = stack.firstOrNull()

    fun pop(): Node? {

        val peek = peek
        stack = stack.copyOfRange(1, stack.size)

        return peek
    }

    val empty: Boolean
        get() = stack.isEmpty()

}