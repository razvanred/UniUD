package ro.razvan.uniud.compitino2014

object Es3 {

    fun manhattanRec(i: Int, j: Int): Long =
        if (i == 0 || j == 0) {
            1
        } else {
            manhattanRec(i - 1, j) + manhattanRec(i, j - 1)
        }

    @JvmStatic
    fun main(args: Array<String>) {
        println(manhattanRec(4, 5))
    }

}