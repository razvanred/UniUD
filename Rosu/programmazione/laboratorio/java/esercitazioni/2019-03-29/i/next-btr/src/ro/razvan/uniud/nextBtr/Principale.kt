package ro.razvan.uniud.nextBtr

import ro.razvan.uniud.nextBtr.BtrNotation.btrSucc

fun main(){
    testing()
}

private fun testing(){
    println(btrSucc("+") == "+-")
    println(btrSucc("-") == "+")
    println(btrSucc(".") == "+")
}