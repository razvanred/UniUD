package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;

public final class HelloCommand extends NoArgsCommand {

    private @NotNull Outputter outputter;

    @Contract(pure = true)
    @Inject
    public HelloCommand(final @NotNull Outputter outputter) {
        this.outputter = outputter;
    }

    @Override
    public @NotNull Result handleInput() {
        outputter.output("world!");
        return Result.handled();
    }

    @Contract(pure = true)
    @Override
    public @NotNull String key() {
        return "hello";
    }
}
