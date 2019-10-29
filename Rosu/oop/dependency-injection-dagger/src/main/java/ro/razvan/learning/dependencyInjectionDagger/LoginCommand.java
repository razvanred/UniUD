package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.util.Optional;

public class LoginCommand extends SingleArgCommand {
    private final @NotNull Database database;
    private final @NotNull Outputter outputter;
    private final @NotNull UserCommandsRouter.Factory userCommandsRouterFactory;
    private final @NotNull Optional<Account> account;

    @Inject
    public LoginCommand(
            final @NotNull Database database,
            final @NotNull Outputter outputter,
            final @NotNull UserCommandsRouter.Factory userCommandsRouterFactory,
            final @NotNull Optional<Account> account
    ) {
        this.database = database;
        this.outputter = outputter;
        this.userCommandsRouterFactory = userCommandsRouterFactory;
        this.account = account;
    }

    @Override
    protected @NotNull Result handleArg(final @NotNull String arg) {
        if (account.isPresent()) {
            final @NotNull String loggedInUser = account.get().username();
            outputter.output(loggedInUser + " is already logged in.");
            if (!loggedInUser.equals(arg)) {
                outputter.output("run `logout` first before trying to log in with another user.");
            }
            return Result.handled();
        }
        outputter.output("Logging in " + arg);
        return Result.enterNestedCommandSet(userCommandsRouterFactory.create(arg).router());
    }

    @Override
    public @NotNull String key() {
        return "login";
    }
}
