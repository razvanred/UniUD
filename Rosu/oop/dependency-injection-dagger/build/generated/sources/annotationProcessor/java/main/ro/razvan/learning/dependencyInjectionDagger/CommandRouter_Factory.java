package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
import java.util.Map;
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
public final class CommandRouter_Factory implements Factory<CommandRouter> {
  private final Provider<Map<String, Command>> commandsProvider;

  private final Provider<Outputter> outputterProvider;

  public CommandRouter_Factory(Provider<Map<String, Command>> commandsProvider,
      Provider<Outputter> outputterProvider) {
    this.commandsProvider = commandsProvider;
    this.outputterProvider = outputterProvider;
  }

  @Override
  public CommandRouter get() {
    return newInstance(commandsProvider.get(), outputterProvider.get());
  }

  public static CommandRouter_Factory create(Provider<Map<String, Command>> commandsProvider,
      Provider<Outputter> outputterProvider) {
    return new CommandRouter_Factory(commandsProvider, outputterProvider);
  }

  public static CommandRouter newInstance(Map<String, Command> commands, Outputter outputter) {
    return new CommandRouter(commands, outputter);
  }
}
