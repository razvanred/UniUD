package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

public abstract class NoArgsCommand implements Command {

    @Override
    public @NotNull Result handleInput(final @NotNull String[] input) {
        return input.length != 0 ? Result.invalid() : handleInput();
    }

    protected abstract @NotNull Result handleInput();
}
