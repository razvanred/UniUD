package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.DoubleCheck;
import dagger.internal.InstanceFactory;
import dagger.internal.MapBuilder;
import dagger.internal.MapFactory;
import dagger.internal.Preconditions;
import java.util.Map;
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
public final class DaggerCommandProcessorFactory implements CommandProcessorFactory {
  /**
   * A {@link Provider} that returns {@code Optional.empty()}. */
  @SuppressWarnings("rawtypes")
  private static final Provider ABSENT_JDK_OPTIONAL_PROVIDER = InstanceFactory.create(Optional.empty());

  private Provider<HelloCommand> helloCommandProvider;

  private Provider<Database> databaseProvider;

  private Provider<UserCommandsRouter.Factory> userCommandsRouterFactoryProvider;

  private Provider<Optional<Account>> optionalOfAccountProvider;

  private Provider<LoginCommand> loginCommandProvider;

  private Provider<Map<String, Command>> mapOfStringAndCommandProvider;

  private Provider<CommandRouter> commandRouterProvider;

  private Provider<CommandProcessor> commandProcessorProvider;

  private DaggerCommandProcessorFactory() {

    initialize();
  }

  public static Builder builder() {
    return new Builder();
  }

  public static CommandProcessorFactory create() {
    return new Builder().build();
  }

  private HelloCommand getHelloCommand() {
    return new HelloCommand(SystemOutModule_TextOutputterFactory.textOutputter());}

  @SuppressWarnings("unchecked")
  private void initialize() {
    this.helloCommandProvider = HelloCommand_Factory.create(SystemOutModule_TextOutputterFactory.create());
    this.databaseProvider = DoubleCheck.provider(Database_Factory.create());
    this.userCommandsRouterFactoryProvider = new Provider<UserCommandsRouter.Factory>() {
      @Override
      public UserCommandsRouter.Factory get() {
        return new UserCommandsRouterFactory();}
    };
    this.optionalOfAccountProvider = absentJdkOptionalProvider();
    this.loginCommandProvider = LoginCommand_Factory.create(databaseProvider, SystemOutModule_TextOutputterFactory.create(), userCommandsRouterFactoryProvider, optionalOfAccountProvider);
    this.mapOfStringAndCommandProvider = MapFactory.<String, Command>builder(2).put("hello", (Provider) helloCommandProvider).put("login", (Provider) loginCommandProvider).build();
    this.commandRouterProvider = CommandRouter_Factory.create(mapOfStringAndCommandProvider, SystemOutModule_TextOutputterFactory.create());
    this.commandProcessorProvider = DoubleCheck.provider(CommandProcessor_Factory.create(commandRouterProvider));
  }

  @Override
  public CommandProcessor processor() {
    return commandProcessorProvider.get();}

  /**
   * Returns a {@link Provider} that returns {@code Optional.empty()}. */
  private static <T> Provider<Optional<T>> absentJdkOptionalProvider() {
    @SuppressWarnings("unchecked") // safe covariant cast
    Provider<Optional<T>> provider = (Provider<Optional<T>>) ABSENT_JDK_OPTIONAL_PROVIDER;return provider;}

  public static final class Builder {
    private Builder() {
    }

    public CommandProcessorFactory build() {
      return new DaggerCommandProcessorFactory();
    }
  }

  private final class UserCommandsRouterFactory implements UserCommandsRouter.Factory {
    @Override
    public UserCommandsRouter create(final String username) {
      Preconditions.checkNotNull(username);
      return new UserCommandsRouterImpl(username);
    }
  }

  private final class UserCommandsRouterImpl implements UserCommandsRouter {
    private final String username;

    private Provider<WithdrawalLimiter> withdrawalLimiterProvider;

    private UserCommandsRouterImpl(String usernameParam) {
      this.username = usernameParam;
      initialize(usernameParam);
    }

    private Account getAccount() {
      return AccountModule_AccountFactory.account(DaggerCommandProcessorFactory.this.databaseProvider.get(), username);}

    private LoginCommand getLoginCommand() {
      return new LoginCommand(DaggerCommandProcessorFactory.this.databaseProvider.get(), SystemOutModule_TextOutputterFactory.textOutputter(), new UserCommandsRouterFactory(), Optional.of(getAccount()));}

    private DepositCommand getDepositCommand() {
      return new DepositCommand(getAccount(), withdrawalLimiterProvider.get(), SystemOutModule_TextOutputterFactory.textOutputter());}

    private WithdrawCommand getWithdrawCommand() {
      return new WithdrawCommand(getAccount(), SystemOutModule_TextOutputterFactory.textOutputter(), AmountsModule_MinimumBalanceFactory.minimumBalance(), withdrawalLimiterProvider.get());}

    private LogoutCommand getLogoutCommand() {
      return new LogoutCommand(getAccount(), SystemOutModule_TextOutputterFactory.textOutputter());}

    private BalanceCommand getBalanceCommand() {
      return new BalanceCommand(getAccount(), SystemOutModule_TextOutputterFactory.textOutputter());}

    private Map<String, Command> getMapOfStringAndCommand() {
      return MapBuilder.<String, Command>newMapBuilder(6).put("hello", DaggerCommandProcessorFactory.this.getHelloCommand()).put("login", getLoginCommand()).put("deposit", getDepositCommand()).put("withdraw", getWithdrawCommand()).put("logout", getLogoutCommand()).put("balance", getBalanceCommand()).build();}

    @SuppressWarnings("unchecked")
    private void initialize(final String usernameParam) {
      this.withdrawalLimiterProvider = DoubleCheck.provider(WithdrawalLimiter_Factory.create(AmountsModule_MaximumWithdrawalFactory.create()));
    }

    @Override
    public CommandRouter router() {
      return new CommandRouter(getMapOfStringAndCommand(), SystemOutModule_TextOutputterFactory.textOutputter());}
  }
}
