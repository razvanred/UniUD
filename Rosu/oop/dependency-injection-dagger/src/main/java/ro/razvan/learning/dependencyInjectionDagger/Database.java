package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.HashMap;
import java.util.Map;

@Singleton
public class Database {
    private final @NotNull Map<String, @NotNull Account> accounts = new HashMap<>();

    @Inject
    public Database() {
    }

    public @NotNull Account getAccount(final @NotNull String username) {
        return accounts.computeIfAbsent(username, Account::new);
    }
}
