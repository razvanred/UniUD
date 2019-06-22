package ro.razvan.uniud.stringSList

@Suppress("EqualsOrHashCode")
class StringSList {

    private val empty: Boolean
    private val first: String
    private val rest: StringSList?

    companion object {
        val NULL_STRING_S_LIST = StringSList()
    }

    public constructor() {
        empty = true
        first = ""
        rest = null
    }

    constructor(first: String, sl: StringSList = NULL_STRING_S_LIST) {
        rest = sl
        this.first = first
        empty = false
    }

    fun isNull(): Boolean {
        return empty
    }

    val car: String
        get() = first

    val cdr: StringSList
        get() = rest ?: NULL_STRING_S_LIST

    fun cons(newElement: String): StringSList {
        return if (empty) {
            StringSList(newElement)
        } else {
            return StringSList(newElement, this)
        }
    }

    val length: Int
        get() = if (empty) {
            0
        } else {
            1 + cdr.length
        }

    fun listRef(k: Int): String {
        return if (k == 0) {
            first
        } else {
            cdr.listRef(k - 1)
        }
    }

    override fun equals(other: Any?): Boolean {
        return when {
            other == null || other !is StringSList -> false
            other.isNull() || empty -> empty && other.isNull()
            other.car != car -> false
            else -> other.cdr == cdr
        }
    }

    fun append(sl: StringSList): StringSList {
        return if (empty) {
            sl
        } else {
            cdr.append(sl).cons(first)
        }
    }

    fun reverse(): StringSList {
        return reverseRec(StringSList())
    }

    private fun reverseRec(sl: StringSList): StringSList {
        return if (empty) {
            sl
        } else {
            cdr.reverseRec(sl.cons(first))
        }
    }

    override fun toString(): String {

        return when {
            empty -> "()"
            cdr.empty -> "($first)"
            else -> {
                val builder = StringBuilder("($first")
                var p = cdr

                while (!p.isNull()) {
                    builder.append(", ${p.first}")
                    p = p.cdr
                }

                builder.append(")").toString()
            }
        }

    }

}