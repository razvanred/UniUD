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
public final class DepositCommand_Factory implements Factory<DepositCommand> {
  private final Provider<Account> accountProvider;

  private final Provider<WithdrawalLimiter> withdrawalLimiterProvider;

  private final Provider<Outputter> outputterProvider;

  public DepositCommand_Factory(Provider<Account> accountProvider,
      Provider<WithdrawalLimiter> withdrawalLimiterProvider,
      Provider<Outputter> outputterProvider) {
    this.accountProvider = accountProvider;
    this.withdrawalLimiterProvider = withdrawalLimiterProvider;
    this.outputterProvider = outputterProvider;
  }

  @Override
  public DepositCommand get() {
    return newInstance(accountProvider.get(), withdrawalLimiterProvider.get(), outputterProvider.get());
  }

  public static DepositCommand_Factory create(Provider<Account> accountProvider,
      Provider<WithdrawalLimiter> withdrawalLimiterProvider,
      Provider<Outputter> outputterProvider) {
    return new DepositCommand_Factory(accountProvider, withdrawalLimiterProvider, outputterProvider);
  }

  public static DepositCommand newInstance(Account account, WithdrawalLimiter withdrawalLimiter,
      Outputter outputter) {
    return new DepositCommand(account, withdrawalLimiter, outputter);
  }
}
