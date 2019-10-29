package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.math.BigDecimal;

public class DepositCommand extends BigDecimalCommand {

    private final @NotNull Account account;
    private final @NotNull Outputter outputter;
    private final @NotNull WithdrawalLimiter withdrawalLimiter;

    @Inject
    @Contract(pure = true)
    public DepositCommand(
            final @NotNull Account account,
            final @NotNull WithdrawalLimiter withdrawalLimiter,
            final @NotNull Outputter outputter
    ) {
        super(outputter);
        this.account = account;
        this.outputter = outputter;
        this.withdrawalLimiter = withdrawalLimiter;
    }

    @Override
    public @NotNull String key() {
        return "deposit";
    }

    @Override
    protected void handleAmount(final @NotNull BigDecimal amount) {
        account.deposit(amount);
        withdrawalLimiter.recordDeposit(amount);
        outputter.output(account.username() + " now has: " + account.balance());
    }
}
