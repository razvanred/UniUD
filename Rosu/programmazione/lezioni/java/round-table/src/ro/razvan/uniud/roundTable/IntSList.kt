package ro.razvan.uniud.roundTable

@Suppress("EqualsOrHashCode")
class IntSList {

    private val empty: Boolean
    private val rest: IntSList?
    private val first: Int

    companion object {
        val NULL_INT_S_LIST = IntSList()
    }

    constructor() {
        empty = true
        rest = null
        first = 0
    }

    constructor(first: Int, rest: IntSList = NULL_INT_S_LIST) {
        this.first = first
        this.rest = rest
        empty = false
    }

    val car: Int
        get() = first

    val cdr: IntSList
        get() = rest ?: NULL_INT_S_LIST

    val length: Int
        get() = if (empty) {
            0
        } else {
            1 + cdr.length
        }

    fun isNull(): Boolean = empty

    fun cons(element: Int) = IntSList(element, this)

    fun append(list: IntSList): IntSList {
        return if (empty) {
            list
        } else {
            cdr.append(list).cons(first)
        }
    }

    fun reverse() = reverseRec(IntSList())

    private fun reverseRec(list: IntSList): IntSList {
        return if (empty) {
            list
        } else {
            cdr.reverseRec(list.cons(first))
        }
    }

    fun listRef(position: Int): Int {
        return if (position == 0) {
            first
        } else {
            cdr.listRef(position - 1)
        }
    }

    override fun toString(): String {
        return when {
            empty -> "()"
            cdr.isNull() -> "($first)"
            else -> {
                var p = cdr
                val builder = StringBuilder("($first")

                while (!p.isNull()) {
                    builder.append(", ${p.car}")
                    p = p.cdr
                }

                builder.append(")").toString()
            }
        }
    }

    override fun equals(other: Any?): Boolean {

        return when {
            other == null || other !is IntSList -> false
            other.empty || empty -> empty && other.empty
            other.first != first -> false
            else -> other.cdr == cdr
        }

    }

}