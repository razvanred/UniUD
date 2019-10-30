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
public final class HelloCommand_Factory implements Factory<HelloCommand> {
  private final Provider<Outputter> outputterProvider;

  public HelloCommand_Factory(Provider<Outputter> outputterProvider) {
    this.outputterProvider = outputterProvider;
  }

  @Override
  public HelloCommand get() {
    return newInstance(outputterProvider.get());
  }

  public static HelloCommand_Factory create(Provider<Outputter> outputterProvider) {
    return new HelloCommand_Factory(outputterProvider);
  }

  public static HelloCommand newInstance(Outputter outputter) {
    return new HelloCommand(outputter);
  }
}
