package ro.razvan.uniud.oop.telephoneOperator.di;

import dagger.internal.DoubleCheck;
import dagger.internal.Preconditions;
import javax.annotation.processing.Generated;
import javax.inject.Provider;
import ro.razvan.uniud.oop.telephoneOperator.model.App;
import ro.razvan.uniud.oop.telephoneOperator.model.App_Factory;
import ro.razvan.uniud.oop.telephoneOperator.model.App_MembersInjector;
import ro.razvan.uniud.oop.telephoneOperator.model.TelephoneOperator;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.PhoneNumberGeneratorModule_PhoneNumberGeneratorFactory;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.PrefixGeneratorModule_PrefixGeneratorFactory;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class DaggerAppComponent implements AppComponent {
  private Provider<RandomUniqueNumbersGenerator> prefixGeneratorProvider;

  private DaggerAppComponent() {

    initialize();
  }

  public static Builder builder() {
    return new Builder();
  }

  public static AppComponent create() {
    return new Builder().build();
  }

  @SuppressWarnings("unchecked")
  private void initialize() {
    this.prefixGeneratorProvider = DoubleCheck.provider(PrefixGeneratorModule_PrefixGeneratorFactory.create());
  }

  @Override
  public App app() {
    return injectApp(App_Factory.newInstance(ConsoleModule_OutputFactory.output(), ConsoleModule_ErrorFactory.error(), prefixGeneratorProvider.get(), new TelephoneOperatorComponentFactory()));}

  private App injectApp(App instance) {
    App_MembersInjector.injectExecute(instance);
    return instance;
  }

  public static final class Builder {
    private Builder() {
    }

    public AppComponent build() {
      return new DaggerAppComponent();
    }
  }

  private final class TelephoneOperatorComponentFactory implements TelephoneOperatorComponent.Factory {
    @Override
    public TelephoneOperatorComponent create(final String prefix, final String name) {
      Preconditions.checkNotNull(prefix);
      Preconditions.checkNotNull(name);
      return new TelephoneOperatorComponentImpl(prefix, name);
    }
  }

  private final class TelephoneOperatorComponentImpl implements TelephoneOperatorComponent {
    private final String prefix;

    private final String name;

    private Provider<RandomUniqueNumbersGenerator> phoneNumberGeneratorProvider;

    private TelephoneOperatorComponentImpl(String prefixParam, String nameParam) {
      this.prefix = prefixParam;
      this.name = nameParam;
      initialize(prefixParam, nameParam);
    }

    @SuppressWarnings("unchecked")
    private void initialize(final String prefixParam, final String nameParam) {
      this.phoneNumberGeneratorProvider = DoubleCheck.provider(PhoneNumberGeneratorModule_PhoneNumberGeneratorFactory.create());
    }

    @Override
    public TelephoneOperator operator() {
      return new TelephoneOperator(prefix, name, phoneNumberGeneratorProvider.get());}
  }
}
