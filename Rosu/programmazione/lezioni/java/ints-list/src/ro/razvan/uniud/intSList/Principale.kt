package ro.razvan.uniud.intSList

fun main() {
    // (cons 34 (cons 56 (const 12 (cons 23 (list)))))
    val list = IntSList(34, IntSList().cons(23).cons(12).cons(56))
    println("testing toString: ${list.toString() == "(34, 56, 12, 23)"}")
    println("testing length: ${list.length == 4}")
    println("testing append: ${IntSList(56).append(list) == IntSList(56, list)}")
    println("testing reverse: ${list.reverse() == IntSList(23, IntSList(12, IntSList(34).cons(56)))}")
    println("testing listRef: ${list.listRef(list.length - 1) == 23}")
}