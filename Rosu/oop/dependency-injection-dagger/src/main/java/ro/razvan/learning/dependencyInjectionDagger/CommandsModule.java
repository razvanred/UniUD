package ro.razvan.learning.dependencyInjectionDagger;

import dagger.Binds;
import dagger.BindsOptionalOf;
import dagger.Module;
import dagger.multibindings.IntoMap;
import dagger.multibindings.StringKey;

@Module
public interface CommandsModule {

    @Binds
    @IntoMap
    @StringKey("hello")
    Command helloCommand(final HelloCommand command);

    @Binds
    @IntoMap
    @StringKey("login")
    Command loginCommand(final LoginCommand command);

    @BindsOptionalOf
    Account optionalAccount();

}
