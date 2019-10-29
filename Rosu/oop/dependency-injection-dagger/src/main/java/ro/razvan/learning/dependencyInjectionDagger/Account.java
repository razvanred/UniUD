package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.math.BigDecimal;

public class Account {
    private final @NotNull String username;
    private @NotNull BigDecimal balance;

    @Contract(pure = true)
    public Account(final @NotNull String username) {
        this.username = username;
        balance = BigDecimal.ZERO;
    }

    public @NotNull BigDecimal balance() {
        return balance;
    }

    public @NotNull String username() {
        return username;
    }

    public void deposit(final @NotNull BigDecimal amount) {
        balance = balance.add(amount);
    }

    public void withdraw(final @NotNull BigDecimal amount) {
        balance = balance.subtract(amount);
    }
}
