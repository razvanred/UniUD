package ro.razvan.uniud.oop.telephoneOperator.printer;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.parser.SimCardParser;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

import javax.inject.Inject;

public final class SimCardPrinter {
    private final @NotNull
    SimCardParser parser;

    private final @NotNull
    Outputter outputter;

    @Inject
    public SimCardPrinter(
            final @NotNull SimCardParser parser,
            @SimCardPrinterOutputter final @NotNull Outputter outputter
    ) {
        this.parser = parser;
        this.outputter = outputter;
    }

    public void print() {
        outputter.output(parser.parse());
    }
}
