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
public final class AmountsModule_MaximumWithdrawalFactory implements Factory<BigDecimal> {
  @Override
  public BigDecimal get() {
    return maximumWithdrawal();
  }

  public static AmountsModule_MaximumWithdrawalFactory create() {
    return InstanceHolder.INSTANCE;
  }

  public static BigDecimal maximumWithdrawal() {
    return Preconditions.checkNotNull(AmountsModule.maximumWithdrawal(), "Cannot return null from a non-@Nullable @Provides method");
  }

  private static final class InstanceHolder {
    private static final AmountsModule_MaximumWithdrawalFactory INSTANCE = new AmountsModule_MaximumWithdrawalFactory();
  }
}
