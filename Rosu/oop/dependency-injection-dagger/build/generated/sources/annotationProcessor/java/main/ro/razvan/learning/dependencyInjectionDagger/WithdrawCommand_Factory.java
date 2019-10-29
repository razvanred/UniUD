package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
import java.math.BigDecimal;
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
public final class WithdrawCommand_Factory implements Factory<WithdrawCommand> {
  private final Provider<Account> accountProvider;

  private final Provider<Outputter> outputterProvider;

  private final Provider<BigDecimal> minimumBalanceProvider;

  private final Provider<WithdrawalLimiter> withdrawalLimiterProvider;

  public WithdrawCommand_Factory(Provider<Account> accountProvider,
      Provider<Outputter> outputterProvider, Provider<BigDecimal> minimumBalanceProvider,
      Provider<WithdrawalLimiter> withdrawalLimiterProvider) {
    this.accountProvider = accountProvider;
    this.outputterProvider = outputterProvider;
    this.minimumBalanceProvider = minimumBalanceProvider;
    this.withdrawalLimiterProvider = withdrawalLimiterProvider;
  }

  @Override
  public WithdrawCommand get() {
    return newInstance(accountProvider.get(), outputterProvider.get(), minimumBalanceProvider.get(), withdrawalLimiterProvider.get());
  }

  public static WithdrawCommand_Factory create(Provider<Account> accountProvider,
      Provider<Outputter> outputterProvider, Provider<BigDecimal> minimumBalanceProvider,
      Provider<WithdrawalLimiter> withdrawalLimiterProvider) {
    return new WithdrawCommand_Factory(accountProvider, outputterProvider, minimumBalanceProvider, withdrawalLimiterProvider);
  }

  public static WithdrawCommand newInstance(Account account, Outputter outputter,
      BigDecimal minimumBalance, WithdrawalLimiter withdrawalLimiter) {
    return new WithdrawCommand(account, outputter, minimumBalance, withdrawalLimiter);
  }
}
