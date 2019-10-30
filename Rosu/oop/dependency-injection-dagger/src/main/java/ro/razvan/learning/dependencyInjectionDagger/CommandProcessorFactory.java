package ro.razvan.learning.dependencyInjectionDagger;

import dagger.Component;
import org.jetbrains.annotations.NotNull;

import javax.inject.Singleton;

@Singleton
@Component(
        modules = {
                CommandsModule.class,
                SystemOutModule.class,
                UserCommandsRouter.InstallationModule.class
        }
)
public interface CommandProcessorFactory {
    CommandProcessor processor();

    static @NotNull CommandProcessorFactory create() {
        return DaggerCommandProcessorFactory.create();
    }
}
