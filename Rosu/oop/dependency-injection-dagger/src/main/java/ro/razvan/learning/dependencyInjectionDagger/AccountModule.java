package ro.razvan.learning.dependencyInjectionDagger;

import dagger.Module;
import dagger.Provides;

@Module
public interface AccountModule {
    @Provides
    static Account account(final Database database, @Username final String username) {
        return database.getAccount(username);
    }
}
