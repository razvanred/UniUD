package ro.razvan.uniud.oop.telephoneOperator.model;

import dagger.internal.Factory;
import javax.annotation.processing.Generated;
import javax.inject.Provider;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class TelephoneOperator_Factory implements Factory<TelephoneOperator> {
  private final Provider<String> prefixProvider;

  private final Provider<String> nameProvider;

  private final Provider<RandomUniqueNumbersGenerator> phoneNumberGeneratorProvider;

  public TelephoneOperator_Factory(Provider<String> prefixProvider, Provider<String> nameProvider,
      Provider<RandomUniqueNumbersGenerator> phoneNumberGeneratorProvider) {
    this.prefixProvider = prefixProvider;
    this.nameProvider = nameProvider;
    this.phoneNumberGeneratorProvider = phoneNumberGeneratorProvider;
  }

  @Override
  public TelephoneOperator get() {
    return newInstance(prefixProvider.get(), nameProvider.get(), phoneNumberGeneratorProvider.get());
  }

  public static TelephoneOperator_Factory create(Provider<String> prefixProvider,
      Provider<String> nameProvider,
      Provider<RandomUniqueNumbersGenerator> phoneNumberGeneratorProvider) {
    return new TelephoneOperator_Factory(prefixProvider, nameProvider, phoneNumberGeneratorProvider);
  }

  public static TelephoneOperator newInstance(String prefix, String name,
      RandomUniqueNumbersGenerator phoneNumberGenerator) {
    return new TelephoneOperator(prefix, name, phoneNumberGenerator);
  }
}
