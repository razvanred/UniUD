package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.inject.Inject;
import java.util.Arrays;
import java.util.Map;

public final class CommandRouter {

    private final @NotNull Map<String, @NotNull Command> commands;
    private final @NotNull Outputter outputter;

    @Contract(pure = true)
    @Inject
    public CommandRouter(
            final @NotNull Map<String, @NotNull Command> commands,
            final @NotNull Outputter outputter
    ) {
        this.commands = commands;
        this.outputter = outputter;
    }

    public @NotNull Command.Result route(final @NotNull String input) {
        final @NotNull String[] splitInput = split(input);

        if (splitInput.length == 0) {
            return invalidCommand(input);
        }

        final @NotNull String commandKey = splitInput[0];
        final @Nullable Command command = commands.get(commandKey);

        if (command == null) {
            return invalidCommand(input);
        }

        final @NotNull Command.Result result = command.handleInput(Arrays.copyOfRange(splitInput, 1, splitInput.length));

        if (result.status() == Command.Status.INVALID) {
            outputter.output(commandKey + ": invalid arguments.");
        }

        return result;
    }

    private @NotNull Command.Result invalidCommand(final @NotNull String input) {
        outputter.output(String.format("Can't understand \"%s\". Please try again.", input));
        return Command.Result.invalid();
    }

    private static @NotNull String[] split(final @NotNull String input) {
        return input.trim().split(" ");
    }

}
