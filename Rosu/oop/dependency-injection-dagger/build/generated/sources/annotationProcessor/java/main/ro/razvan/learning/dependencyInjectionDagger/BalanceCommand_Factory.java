package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
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
public final class BalanceCommand_Factory implements Factory<BalanceCommand> {
  private final Provider<Account> accountProvider;

  private final Provider<Outputter> outputterProvider;

  public BalanceCommand_Factory(Provider<Account> accountProvider,
      Provider<Outputter> outputterProvider) {
    this.accountProvider = accountProvider;
    this.outputterProvider = outputterProvider;
  }

  @Override
  public BalanceCommand get() {
    return newInstance(accountProvider.get(), outputterProvider.get());
  }

  public static BalanceCommand_Factory create(Provider<Account> accountProvider,
      Provider<Outputter> outputterProvider) {
    return new BalanceCommand_Factory(accountProvider, outputterProvider);
  }

  public static BalanceCommand newInstance(Account account, Outputter outputter) {
    return new BalanceCommand(account, outputter);
  }
}
