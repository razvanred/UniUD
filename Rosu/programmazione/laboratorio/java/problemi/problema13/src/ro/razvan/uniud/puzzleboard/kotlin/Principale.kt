package ro.razvan.uniud.puzzleboard.kotlin

import ro.razvan.uniud.puzzleboard.DefaultValues
import ro.razvan.uniud.puzzleboard.kotlin.util.Injector
import kotlin.math.sqrt

/**
 * Sono 3 le possibili configurazioni degli argomenti:
 *
 * <ul>
 * <li>Con la lista vuota degli argomenti vuota verrà scelta una dimensione di default dall'oggetto DefaultValues</li>
 * <li>Se la lista ha un solo argomento verrà interpretato come la dimensione della tavoletta</li>
 * <li>Se invece vengono passati più di un argomento verranno considerati come la configurazione della tavoletta</li>
 * </ul>
 *
 * @param args lista di argomenti che rispettano le 3 regole sopra indicate
 */
fun main(args: Array<String>) {

    if (args.size <= 1) {

        val n = if (args.isEmpty()) DefaultValues.N else args[1].toInt()

        Injector.start(n)

        with(Injector.console) {
            shuffle()
            play()
        }

    } else {

        val n = sqrt(args.size.toDouble())

        val illegalArgument = IllegalArgumentException("The given numbers are invalid")

        if (n % 1 != 0.toDouble()) {

            throw illegalArgument

        } else {

            val array = args.map(String::toInt).toIntArray()

            array.sortedArray().forEachIndexed { i, element ->
                if (element != i + 1) {
                    throw illegalArgument
                }
            }

            Injector.start(
                array
            )

            Injector.console.play()
        }

    }

}