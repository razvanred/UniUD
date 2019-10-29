package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Objects;

@Singleton
public final class CommandProcessor {
    private final @NotNull Deque<@NotNull CommandRouter> commandRouterStack = new ArrayDeque<>();

    @Inject
    public CommandProcessor(final @NotNull CommandRouter firstCommandRouter) {
        commandRouterStack.push(firstCommandRouter);
    }

    public @NotNull Command.Status process(final @NotNull String input) {
        final @NotNull Command.Result result = Objects.requireNonNull(commandRouterStack.peek()).route(input);

        if (result.status() == Command.Status.INPUT_COMPLETED) {
            commandRouterStack.pop();
            return commandRouterStack.isEmpty() ? Command.Status.INPUT_COMPLETED : Command.Status.HANDLED;
        }

        final var nestedCommandRouter = result.nestedCommandRouter();
        if (nestedCommandRouter != null) {
            commandRouterStack.push(nestedCommandRouter);
        }

        return result.status();
    }

}
