package ro.razvan.uniud.oop.telephoneOperator.di;

import dagger.BindsInstance;
import dagger.Module;
import dagger.Subcomponent;
import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.TelephoneOperator;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.PhoneNumberGeneratorModule;

@TelephoneOperatorScope
@Subcomponent(modules = PhoneNumberGeneratorModule.class)
public interface TelephoneOperatorComponent {
    TelephoneOperator operator();

    @Subcomponent.Factory
    interface Factory {

        TelephoneOperatorComponent create(
                @BindsInstance @Prefix final @NotNull String prefix,
                @BindsInstance @OperatorName final @NotNull String name
        );
    }

    @Module(subcomponents = {TelephoneOperatorComponent.class})
    interface InstallationModule {
    }
}
