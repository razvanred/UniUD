package ro.razvan.uniud.oop.telephoneOperator.di;

import dagger.internal.Factory;
import dagger.internal.Preconditions;
import javax.annotation.processing.Generated;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class ConsoleModule_OutputFactory implements Factory<Outputter> {
  @Override
  public Outputter get() {
    return output();
  }

  public static ConsoleModule_OutputFactory create() {
    return InstanceHolder.INSTANCE;
  }

  public static Outputter output() {
    return Preconditions.checkNotNull(ConsoleModule.output(), "Cannot return null from a non-@Nullable @Provides method");
  }

  private static final class InstanceHolder {
    private static final ConsoleModule_OutputFactory INSTANCE = new ConsoleModule_OutputFactory();
  }
}
