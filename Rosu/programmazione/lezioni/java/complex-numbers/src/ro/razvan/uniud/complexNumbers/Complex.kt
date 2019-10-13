package ro.razvan.uniud.complexNumbers

class Complex {

    companion object {
        private const val IMAGINARY_SYMBOL = 'i'
        private const val MINUS_CHAR = '-'
        private const val PLUS_CHAR = '+'
    }

    private val x: Double
    private val y: Double

    constructor(x: Double, y: Double) {
        this.x = x
        this.y = y
    }

    constructor(complex: String) {
        val k = complex.indexOf(IMAGINARY_SYMBOL)

        when {
            k < 0 -> { // i non compare, solo la parte reale

                x = complex.toDouble()
                y = 0.toDouble()

            }
            k == 0 -> { // i all'inizio, solo parte immaginaria

                x = 0.toDouble()
                y = complex.substring(1).toDouble()

            }
            k == 1 && complex[0] == MINUS_CHAR -> { // solo parte immaginaria, i preceduto da segno -

                x = 0.toDouble()
                y = -complex.substring(2).toDouble()

            }
            k == 1 && complex[0] == PLUS_CHAR -> { // solo parte immaginaria, i preceduto da segno +

                x = 0.toDouble()
                y = complex.substring(2).toDouble()

            }
            else -> { // parte reale e parte immaginaria

                x = complex.substring(0, k - 1).toDouble()

                y = complex.substring(k + 1).toDouble() * if (complex[k - 1] == MINUS_CHAR) {
                    -1
                } else {
                    1
                }

            }
        }
    }

    fun add(complex: Complex): Complex {
        return Complex(x + complex.x, y + complex.y)
    }

    fun sub(complex: Complex): Complex {
        return Complex(x - complex.x, y - complex.y)
    }

    fun mul(complex: Complex): Complex {
        return Complex(
            x = x * complex.x - y * complex.y,
            y = x * complex.y + y * complex.x
        )
    }

    fun div(complex: Complex): Complex {
        val v = Math.pow(complex.x, 2.toDouble()) + Math.pow(complex.y, 2.toDouble())
        return Complex(
            x = (x * complex.x + y * complex.y) / v,
            y = (y * complex.x - x * complex.y) / v
        )
    }

    override fun toString(): String {
        return if (y < 0) {
            "$x-i${-y}"
        } else {
            "$x+i$y"
        }
    }

}