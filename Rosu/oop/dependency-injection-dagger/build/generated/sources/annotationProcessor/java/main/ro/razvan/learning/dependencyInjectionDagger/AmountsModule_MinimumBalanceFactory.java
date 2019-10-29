package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
import dagger.internal.Preconditions;
import java.math.BigDecimal;
import javax.annotation.processing.Generated;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class AmountsModule_MinimumBalanceFactory implements Factory<BigDecimal> {
  @Override
  public BigDecimal get() {
    return minimumBalance();
  }

  public static AmountsModule_MinimumBalanceFactory create() {
    return InstanceHolder.INSTANCE;
  }

  public static BigDecimal minimumBalance() {
    return Preconditions.checkNotNull(AmountsModule.minimumBalance(), "Cannot return null from a non-@Nullable @Provides method");
  }

  private static final class InstanceHolder {
    private static final AmountsModule_MinimumBalanceFactory INSTANCE = new AmountsModule_MinimumBalanceFactory();
  }
}
