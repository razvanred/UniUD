package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;

public final class LogoutCommand extends NoArgsCommand {

    private final @NotNull Account account;
    private final @NotNull Outputter outputter;

    @Inject
    public LogoutCommand(final @NotNull Account account, final @NotNull Outputter outputter) {
        this.outputter = outputter;
        this.account = account;
    }

    @Override
    protected @NotNull Result handleInput() {
        outputter.output("Logging out " + account.username());
        return Result.inputCompleted();
    }

    @Contract(pure = true)
    @Override
    public @NotNull String key() {
        return "logout";
    }
}
