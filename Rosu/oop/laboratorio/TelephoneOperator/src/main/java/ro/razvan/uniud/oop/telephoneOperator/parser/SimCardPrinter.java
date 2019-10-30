package ro.razvan.uniud.oop.telephoneOperator.parser;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

public class SimCardPrinter {
    private final @NotNull
    SimCardParser parser;

    private final @NotNull
    Outputter outputter;

    public SimCardPrinter(
            final @NotNull SimCardParser parser,
            final @NotNull Outputter outputter
    ) {
        this.parser = parser;
        this.outputter = outputter;
    }

    public void print() {
        outputter.output(parser.parse());
    }
}
