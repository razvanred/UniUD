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
public final class LogoutCommand_Factory implements Factory<LogoutCommand> {
  private final Provider<Account> accountProvider;

  private final Provider<Outputter> outputterProvider;

  public LogoutCommand_Factory(Provider<Account> accountProvider,
      Provider<Outputter> outputterProvider) {
    this.accountProvider = accountProvider;
    this.outputterProvider = outputterProvider;
  }

  @Override
  public LogoutCommand get() {
    return newInstance(accountProvider.get(), outputterProvider.get());
  }

  public static LogoutCommand_Factory create(Provider<Account> accountProvider,
      Provider<Outputter> outputterProvider) {
    return new LogoutCommand_Factory(accountProvider, outputterProvider);
  }

  public static LogoutCommand newInstance(Account account, Outputter outputter) {
    return new LogoutCommand(account, outputter);
  }
}
