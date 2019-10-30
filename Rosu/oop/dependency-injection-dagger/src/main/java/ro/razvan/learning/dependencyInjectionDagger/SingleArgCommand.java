package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

public abstract class SingleArgCommand implements Command {

    @Override
    public @NotNull Result handleInput(final @NotNull String[] input) {
        return input.length != 1 ? Result.invalid() : handleArg(input[0]);
    }

    protected abstract @NotNull Result handleArg(final @NotNull String arg);
}
