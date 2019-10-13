package ro.razvan.uniud.generics

fun main() {
    println("testing empty constructor and length: ${SList<String>().length() == 0}")
    println(
        "testing primary constructor and cons: ${SList(
            "sono",
            SList("razvan")
        ) == SList("razvan").cons("sono")}"
    )
    println("testing toString with empty list: ${SList<String>().toString() == "()"}")
    println("testing toString with list: ${SList("razvan").toString() == "(razvan)"}")
    println("testing length: ${SList("razvan").cons("rosu").length() == 2}")
    println("testing car: ${SList("madalina", SList("razvan")).car() == "madalina"}")
    println(
        "testing cdr: ${SList(
            "stefan",
            SList("rosu").cons("alexandru")
        ).cdr() == SList("rosu").cons("alexandru")}"
    )
    println("testing listRef: ${SList("world").cons("hello").listRef(1) == "world"}")

    val listA = SList("razvan", SList("rosu"))
    val listB = SList("emma", SList("rosu", SList("violetta")))

    println("testing append: ${listA.append(listB).toString() == "(razvan, rosu, emma, rosu, violetta)"}")
    println("testing reverse: ${listB.reverse().toString() == "(violetta, rosu, emma)"}")
}