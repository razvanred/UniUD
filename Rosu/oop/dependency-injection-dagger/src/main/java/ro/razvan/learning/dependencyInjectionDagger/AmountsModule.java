package ro.razvan.learning.dependencyInjectionDagger;

import dagger.Module;
import dagger.Provides;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.math.BigDecimal;

@Module
public interface AmountsModule {

    @Contract(pure = true)
    @Provides
    static @MinimumBalance
    BigDecimal minimumBalance() {
        return BigDecimal.ZERO;
    }

    @NotNull
    @Contract(value = " -> new", pure = true)
    @Provides
    static @MaximumWithdrawal
    BigDecimal maximumWithdrawal() {
        return new BigDecimal(1_000);
    }
}
