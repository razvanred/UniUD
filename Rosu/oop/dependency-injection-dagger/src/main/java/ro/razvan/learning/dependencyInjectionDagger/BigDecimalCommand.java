package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.math.BigDecimal;

public abstract class BigDecimalCommand extends SingleArgCommand {

    private final @NotNull Outputter outputter;

    protected BigDecimalCommand(final @NotNull Outputter outputter) {
        this.outputter = outputter;
    }

    @Override
    protected @NotNull Result handleArg(@NotNull String arg) {
        final @Nullable BigDecimal amount = tryParse(arg);

        if (amount == null) {
            outputter.output(arg + " is not a valid number.");
        } else if (amount.signum() <= 0) {
            outputter.output("Amount must be positive.");
        } else {
            handleAmount(amount);
        }

        return Result.handled();
    }

    private static @Nullable BigDecimal tryParse(final @NotNull String arg) {
        try {
            return new BigDecimal(arg);
        } catch (final @NotNull NumberFormatException exc) {
            return null;
        }
    }

    protected abstract void handleAmount(final @NotNull BigDecimal amount);
}
