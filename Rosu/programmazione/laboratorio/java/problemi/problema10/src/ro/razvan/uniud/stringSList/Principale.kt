package ro.razvan.uniud.stringSList

import ro.razvan.uniud.stringSList.java.BtrNotation
import ro.razvan.uniud.stringSList.kotlin.StringSList

fun main() {
    println("testing empty constructor and length: ${StringSList().length == 0}")
    println(
        "testing primary constructor and cons: ${StringSList(
            "sono",
            StringSList("razvan")
        ) == StringSList("razvan").cons("sono")}"
    )
    println("testing toString with empty list: ${StringSList.NULL_STRING_S_LIST.toString() == "()"}")
    println("testing toString with list: ${StringSList("razvan").toString() == "(razvan)"}")
    println("testing length: ${StringSList("razvan").cons("rosu").length == 2}")
    println("testing car: ${StringSList(
        "madalina",
        StringSList("razvan")
    ).car == "madalina"}")
    println(
        "testing cdr: ${StringSList(
            "stefan",
            StringSList("rosu").cons("alexandru")
        ).cdr == StringSList("rosu").cons("alexandru")}"
    )
    println("testing listRef: ${StringSList("world").cons("hello").listRef(1) == "world"}")

    val listA =
        StringSList("razvan", StringSList("rosu"))
    val listB = StringSList(
        "emma",
        StringSList(
            "rosu",
            StringSList("violetta")
        )
    )

    println("testing append: ${listA.append(listB).toString() == "(razvan, rosu, emma, rosu, violetta)"}")
    println("testing reverse: ${listB.reverse().toString() == "(violetta, rosu, emma)"}")

    println(
        "testing btrNotation: ${BtrNotation.btrSucc(
            "-",
            5
        ) == StringSList("+.").cons("+-").cons("+").cons(".").cons("-")}"
    )
}