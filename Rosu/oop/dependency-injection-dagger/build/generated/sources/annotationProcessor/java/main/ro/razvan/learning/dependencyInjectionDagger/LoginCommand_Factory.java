package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
import java.util.Optional;
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
public final class LoginCommand_Factory implements Factory<LoginCommand> {
  private final Provider<Database> databaseProvider;

  private final Provider<Outputter> outputterProvider;

  private final Provider<UserCommandsRouter.Factory> userCommandsRouterFactoryProvider;

  private final Provider<Optional<Account>> accountProvider;

  public LoginCommand_Factory(Provider<Database> databaseProvider,
      Provider<Outputter> outputterProvider,
      Provider<UserCommandsRouter.Factory> userCommandsRouterFactoryProvider,
      Provider<Optional<Account>> accountProvider) {
    this.databaseProvider = databaseProvider;
    this.outputterProvider = outputterProvider;
    this.userCommandsRouterFactoryProvider = userCommandsRouterFactoryProvider;
    this.accountProvider = accountProvider;
  }

  @Override
  public LoginCommand get() {
    return newInstance(databaseProvider.get(), outputterProvider.get(), userCommandsRouterFactoryProvider.get(), accountProvider.get());
  }

  public static LoginCommand_Factory create(Provider<Database> databaseProvider,
      Provider<Outputter> outputterProvider,
      Provider<UserCommandsRouter.Factory> userCommandsRouterFactoryProvider,
      Provider<Optional<Account>> accountProvider) {
    return new LoginCommand_Factory(databaseProvider, outputterProvider, userCommandsRouterFactoryProvider, accountProvider);
  }

  public static LoginCommand newInstance(Database database, Outputter outputter,
      UserCommandsRouter.Factory userCommandsRouterFactory, Optional<Account> account) {
    return new LoginCommand(database, outputter, userCommandsRouterFactory, account);
  }
}
