package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;

public final class BalanceCommand extends NoArgsCommand {
    private final @NotNull Account account;
    private final @NotNull Outputter outputter;

    @Inject
    public BalanceCommand(final @NotNull Account account, final @NotNull Outputter outputter) {
        this.outputter = outputter;
        this.account = account;
    }

    @Override
    protected @NotNull Result handleInput() {
        outputter.output("Your balance is: " + account.balance());
        return Result.handled();
    }

    @Contract(pure = true)
    @Override
    public @NotNull String key() {
        return "balance";
    }
}
