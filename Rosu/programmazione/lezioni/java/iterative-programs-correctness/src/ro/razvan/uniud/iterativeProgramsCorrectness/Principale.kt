package ro.razvan.uniud.iterativeProgramsCorrectness

/**
 * Moltiplicazione contadino russo
 */
fun peasantMultiplication(m: Int, n: Int): Int {

    var x = m;
    var y = n;
    var z = 0;

    while (y > 0) {

        // xy+z = mn

        if (y % 2 > 0) {
            z += x
        }

        x *= 2
        y /= 2

    }

    // z = mn

    return z
}

fun sqr(n: Int): Int {

    var x = 0
    var y = 0
    var z = 1

    while(x < n){
        x++
        y+=z
        z+=2
    }

    return y //Post: y = n^2
}

fun main(){
    println(sqr(2))
}