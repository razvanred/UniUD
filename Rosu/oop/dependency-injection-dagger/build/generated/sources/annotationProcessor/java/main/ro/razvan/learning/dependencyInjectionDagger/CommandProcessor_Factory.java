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
public final class CommandProcessor_Factory implements Factory<CommandProcessor> {
  private final Provider<CommandRouter> firstCommandRouterProvider;

  public CommandProcessor_Factory(Provider<CommandRouter> firstCommandRouterProvider) {
    this.firstCommandRouterProvider = firstCommandRouterProvider;
  }

  @Override
  public CommandProcessor get() {
    return newInstance(firstCommandRouterProvider.get());
  }

  public static CommandProcessor_Factory create(
      Provider<CommandRouter> firstCommandRouterProvider) {
    return new CommandProcessor_Factory(firstCommandRouterProvider);
  }

  public static CommandProcessor newInstance(CommandRouter firstCommandRouter) {
    return new CommandProcessor(firstCommandRouter);
  }
}
