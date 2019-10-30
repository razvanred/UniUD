package ro.razvan.uniud.oop.telephoneOperator.model;

import dagger.internal.Factory;
import javax.annotation.processing.Generated;
import javax.inject.Provider;
import ro.razvan.uniud.oop.telephoneOperator.di.TelephoneOperatorComponent;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class App_Factory implements Factory<App> {
  private final Provider<Outputter> outputStreamProvider;

  private final Provider<Outputter> errorStreamProvider;

  private final Provider<RandomUniqueNumbersGenerator> prefixGeneratorProvider;

  private final Provider<TelephoneOperatorComponent.Factory> telephoneOperatorComponentFactoryProvider;

  public App_Factory(Provider<Outputter> outputStreamProvider,
      Provider<Outputter> errorStreamProvider,
      Provider<RandomUniqueNumbersGenerator> prefixGeneratorProvider,
      Provider<TelephoneOperatorComponent.Factory> telephoneOperatorComponentFactoryProvider) {
    this.outputStreamProvider = outputStreamProvider;
    this.errorStreamProvider = errorStreamProvider;
    this.prefixGeneratorProvider = prefixGeneratorProvider;
    this.telephoneOperatorComponentFactoryProvider = telephoneOperatorComponentFactoryProvider;
  }

  @Override
  public App get() {
    App instance = newInstance(outputStreamProvider.get(), errorStreamProvider.get(), prefixGeneratorProvider.get(), telephoneOperatorComponentFactoryProvider.get());
    App_MembersInjector.injectExecute(instance);
    return instance;
  }

  public static App_Factory create(Provider<Outputter> outputStreamProvider,
      Provider<Outputter> errorStreamProvider,
      Provider<RandomUniqueNumbersGenerator> prefixGeneratorProvider,
      Provider<TelephoneOperatorComponent.Factory> telephoneOperatorComponentFactoryProvider) {
    return new App_Factory(outputStreamProvider, errorStreamProvider, prefixGeneratorProvider, telephoneOperatorComponentFactoryProvider);
  }

  public static App newInstance(Outputter outputStream, Outputter errorStream,
      RandomUniqueNumbersGenerator prefixGenerator,
      TelephoneOperatorComponent.Factory telephoneOperatorComponentFactory) {
    return new App(outputStream, errorStream, prefixGenerator, telephoneOperatorComponentFactory);
  }
}
