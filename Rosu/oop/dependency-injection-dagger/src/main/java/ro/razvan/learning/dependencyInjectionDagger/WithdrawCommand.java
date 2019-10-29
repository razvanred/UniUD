package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.math.BigDecimal;

public class WithdrawCommand extends BigDecimalCommand {

    private final @NotNull Account account;
    private final @NotNull Outputter outputter;
    private final @NotNull BigDecimal minimumBalance;
    private final @NotNull WithdrawalLimiter withdrawalLimiter;

    @Inject
    public WithdrawCommand(
            final @NotNull Account account,
            final @NotNull Outputter outputter,
            @MinimumBalance final @NotNull BigDecimal minimumBalance,
            final @NotNull WithdrawalLimiter withdrawalLimiter
    ) {
        super(outputter);
        this.account = account;
        this.outputter = outputter;
        this.minimumBalance = minimumBalance;
        this.withdrawalLimiter = withdrawalLimiter;
    }

    @Override
    protected void handleAmount(final @NotNull BigDecimal amount) {
        final @NotNull BigDecimal remainingWithdrawalLimit = withdrawalLimiter.remainingWithdrawalLimit();
        if (amount.compareTo(remainingWithdrawalLimit) > 0) {
            outputter.output(
                    String.format(
                            "You may not withdraw %s; you may withdraw %s more in this session",
                            amount,
                            remainingWithdrawalLimit
                    )
            );
            return;
        }

        final @NotNull BigDecimal newBalance = account.balance().subtract(amount);
        if (newBalance.compareTo(minimumBalance) < 0) {
            outputter.output("Cannot retrieve without violating the minimum balance. Your balance now is: " + account.balance());
            return;
        }

        account.withdraw(amount);
        withdrawalLimiter.recordWithdrawal(amount);
        outputter.output("Your new balance is: " + account.balance());
    }

    @Override
    public @NotNull String key() {
        return "withdraw";
    }
}
