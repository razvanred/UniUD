package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.math.BigDecimal;

@PerSession
public final class WithdrawalLimiter {
    private @NotNull BigDecimal remainingWithdrawalLimit;

    @Contract(pure = true)
    @Inject
    public WithdrawalLimiter(final @MaximumWithdrawal @NotNull BigDecimal maximumWithdrawal) {
        remainingWithdrawalLimit = maximumWithdrawal;
    }

    void recordDeposit(final @NotNull BigDecimal amount) {
        remainingWithdrawalLimit = remainingWithdrawalLimit.add(amount);
    }

    void recordWithdrawal(final @NotNull BigDecimal amount) {
        remainingWithdrawalLimit = remainingWithdrawalLimit.subtract(amount);
    }

    @Contract(pure = true)
    public @NotNull BigDecimal remainingWithdrawalLimit() {
        return remainingWithdrawalLimit;
    }
}
