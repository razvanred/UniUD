package ro.razvan.uniud.intSList

@Suppress("EqualsOrHashCode")
class IntSList {

    private val empty: Boolean
    private val first: Int
    private val rest: IntSList?

    companion object {
        val NULL_INT_S_LIST = IntSList()
    }

    /**
     * Creazione di una lista vuota
     * Scheme: null
     */
    constructor() {
        empty = true
        first = 0 // valore di irrilevanza in questo caso
        rest = null
    }

    constructor(e: Int) : this(e, NULL_INT_S_LIST)

    /**
     * Creazione di una lista non vuota
     * Scheme: cons
     */
    constructor(e: Int, il: IntSList) {
        rest = il
        first = e
        empty = false
    }

    /**
     * Verifica se la lista è vuota
     * Scheme: null?
     */
    val isNull: Boolean
        get() = empty

    /**
     * Primo elemento della lista
     * Scheme: car
     */
    val car: Int
        get() = first

    /**
     * Ritorna la lista senza il primo elemento
     * Scheme: cdr
     */
    val cdr: IntSList
        get() = rest ?: NULL_INT_S_LIST

    /**
     * Crea una nuova lista con l'elemento e in testa
     * Scheme: cons
     */
    fun cons(e: Int): IntSList {
        return IntSList(e, this)
    }

    /**
     * Ritorna la lunghezza della lista
     * Scheme: length
     */
    val length: Int
        get() = if (isNull) {
            0
        } else {
            1 + cdr.length
        }

    /**
     * Elemento in posizione k
     * Scheme: list-ref
     */
    fun listRef(k: Int): Int {
        return if (k == 0) {
            car
        } else {
            cdr.listRef(k - 1)
        }
    }

    /**
     * Fusione di liste
     * Scheme: append
     */
    fun append(list: IntSList): IntSList {
        return if (isNull) {
            list
        } else {
            cdr.append(list).cons(car)
        }
    }

    /**
     * Reverse della lista
     * Scheme: reverse
     */
    fun reverse(): IntSList {
        return reverseRec(IntSList())
    }

    private fun reverseRec(re: IntSList): IntSList {
        return if (isNull) {
            re
        } else {
            return cdr.reverseRec(re.cons(car))
        }
    }

    override fun equals(other: Any?): Boolean {

        return when {
            other == null || other !is IntSList -> false
            isNull || other.isNull -> isNull && other.isNull
            car == other.car -> cdr == other.cdr
            else -> return false
        }

    }

    override fun toString(): String {

        return when {
            empty -> "()"
            rest == null || rest.isNull -> "($car)"
            else -> {
                val rep = StringBuilder("($first")
                var r = rest

                while (r != null && !r.isNull) {
                    rep.append(", ${r.car}")
                    r = r.cdr
                }

                rep.append(")").toString()
            }
        }

    }

}