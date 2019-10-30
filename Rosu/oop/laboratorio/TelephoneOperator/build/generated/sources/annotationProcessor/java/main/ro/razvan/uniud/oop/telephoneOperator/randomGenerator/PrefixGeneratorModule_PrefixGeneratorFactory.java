package ro.razvan.uniud.oop.telephoneOperator.randomGenerator;

import dagger.internal.Factory;
import dagger.internal.Preconditions;
import javax.annotation.processing.Generated;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class PrefixGeneratorModule_PrefixGeneratorFactory implements Factory<RandomUniqueNumbersGenerator> {
  @Override
  public RandomUniqueNumbersGenerator get() {
    return prefixGenerator();
  }

  public static PrefixGeneratorModule_PrefixGeneratorFactory create() {
    return InstanceHolder.INSTANCE;
  }

  public static RandomUniqueNumbersGenerator prefixGenerator() {
    return Preconditions.checkNotNull(PrefixGeneratorModule.prefixGenerator(), "Cannot return null from a non-@Nullable @Provides method");
  }

  private static final class InstanceHolder {
    private static final PrefixGeneratorModule_PrefixGeneratorFactory INSTANCE = new PrefixGeneratorModule_PrefixGeneratorFactory();
  }
}
