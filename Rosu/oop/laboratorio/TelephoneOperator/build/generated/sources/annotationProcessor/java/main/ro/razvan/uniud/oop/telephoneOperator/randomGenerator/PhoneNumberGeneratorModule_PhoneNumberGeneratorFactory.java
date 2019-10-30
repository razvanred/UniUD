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
public final class PhoneNumberGeneratorModule_PhoneNumberGeneratorFactory implements Factory<RandomUniqueNumbersGenerator> {
  @Override
  public RandomUniqueNumbersGenerator get() {
    return phoneNumberGenerator();
  }

  public static PhoneNumberGeneratorModule_PhoneNumberGeneratorFactory create() {
    return InstanceHolder.INSTANCE;
  }

  public static RandomUniqueNumbersGenerator phoneNumberGenerator() {
    return Preconditions.checkNotNull(PhoneNumberGeneratorModule.phoneNumberGenerator(), "Cannot return null from a non-@Nullable @Provides method");
  }

  private static final class InstanceHolder {
    private static final PhoneNumberGeneratorModule_PhoneNumberGeneratorFactory INSTANCE = new PhoneNumberGeneratorModule_PhoneNumberGeneratorFactory();
  }
}
