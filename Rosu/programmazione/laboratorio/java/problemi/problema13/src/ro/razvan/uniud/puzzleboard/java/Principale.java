package ro.razvan.uniud.puzzleboard.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import ro.razvan.uniud.puzzleboard.DefaultValues;
import ro.razvan.uniud.puzzleboard.java.util.Injector;

import java.util.Arrays;

public class Principale {

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
    public static void main(@NotNull final String[] args) {

        if (args.length <= 1) {

            final var size = args.length == 0 ? DefaultValues.N : Integer.parseInt(args[0]);

            Injector.start(size);

            final var console = Injector.getConsole();

            console.shuffle();
            console.play();

        } else {

            final var n = Math.sqrt(args.length);
            final var illegalArgument = new IllegalArgumentException("The given numbers are invalid");

            if (n % 1 != 0) {

                throw illegalArgument;

            } else {

                final var array = Arrays.stream(args)
                        .mapToInt(Integer::parseInt)
                        .toArray();

                final var sorted = Arrays.stream(array).sorted().toArray();

                for (int i = 0; i < sorted.length; i++) {
                    if (i + 1 != sorted[i]) {
                        throw illegalArgument;
                    }
                }

                Injector.start(array);
                Injector.getConsole().play();

            }

        }

    }

    /**
     * System.out.println accorciato
     *
     * @param any oggetto da stampare su console
     */
    public static void println(@Nullable final Object any) {
        System.out.println(any);
    }

}
