package ro.razvan.uniud.oop.telephoneOperator.telephoneOperator;

import dagger.BindsInstance;
import dagger.Module;
import dagger.Subcomponent;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.phoneNumberGenerator.PhoneNumberGeneratorModule;

@TelephoneOperatorScope
@Subcomponent(modules = {PhoneNumberGeneratorModule.class, PrefixModule.class})
public interface TelephoneOperatorComponent {
    TelephoneOperator operator();

    @Subcomponent.Factory
    interface Factory {
        TelephoneOperatorComponent create(
                @BindsInstance @OperatorName final String name
        );
    }

    @Module(subcomponents = {TelephoneOperatorComponent.class})
    interface InstallationModule {
    }
}
