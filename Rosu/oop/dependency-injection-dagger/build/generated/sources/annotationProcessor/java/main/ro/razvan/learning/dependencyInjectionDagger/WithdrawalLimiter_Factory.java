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
public final class WithdrawalLimiter_Factory implements Factory<WithdrawalLimiter> {
  private final Provider<BigDecimal> maximumWithdrawalProvider;

  public WithdrawalLimiter_Factory(Provider<BigDecimal> maximumWithdrawalProvider) {
    this.maximumWithdrawalProvider = maximumWithdrawalProvider;
  }

  @Override
  public WithdrawalLimiter get() {
    return newInstance(maximumWithdrawalProvider.get());
  }

  public static WithdrawalLimiter_Factory create(Provider<BigDecimal> maximumWithdrawalProvider) {
    return new WithdrawalLimiter_Factory(maximumWithdrawalProvider);
  }

  public static WithdrawalLimiter newInstance(BigDecimal maximumWithdrawal) {
    return new WithdrawalLimiter(maximumWithdrawal);
  }
}
