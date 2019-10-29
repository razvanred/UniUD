package ro.razvan.learning.dependencyInjectionDagger;

import dagger.Binds;
import dagger.Module;
import dagger.multibindings.IntoMap;
import dagger.multibindings.StringKey;

@Module
public interface UserCommandsModule {

    @Binds
    @IntoMap
    @StringKey("deposit")
    Command deposit(final DepositCommand command);

    @Binds
    @IntoMap
    @StringKey("withdraw")
    Command withdraw(final WithdrawCommand command);

    @Binds
    @IntoMap
    @StringKey("logout")
    Command logout(final LogoutCommand command);

    @Binds
    @IntoMap
    @StringKey("balance")
    Command balance(final BalanceCommand command);

}
