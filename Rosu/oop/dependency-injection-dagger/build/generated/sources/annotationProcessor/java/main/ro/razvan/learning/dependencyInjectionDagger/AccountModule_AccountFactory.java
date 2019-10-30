package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
import dagger.internal.Preconditions;
import javax.annotation.processing.Generated;
import javax.inject.Provider;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class AccountModule_AccountFactory implements Factory<Account> {
  private final Provider<Database> databaseProvider;

  private final Provider<String> usernameProvider;

  public AccountModule_AccountFactory(Provider<Database> databaseProvider,
      Provider<String> usernameProvider) {
    this.databaseProvider = databaseProvider;
    this.usernameProvider = usernameProvider;
  }

  @Override
  public Account get() {
    return account(databaseProvider.get(), usernameProvider.get());
  }

  public static AccountModule_AccountFactory create(Provider<Database> databaseProvider,
      Provider<String> usernameProvider) {
    return new AccountModule_AccountFactory(databaseProvider, usernameProvider);
  }

  public static Account account(Database database, String username) {
    return Preconditions.checkNotNull(AccountModule.account(database, username), "Cannot return null from a non-@Nullable @Provides method");
  }
}
