package ro.razvan.uniud.oop.telephoneOperator.printer;

import dagger.BindsInstance;
import dagger.Module;
import dagger.Subcomponent;
import ro.razvan.uniud.oop.telephoneOperator.parser.SimCardParser;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Subcomponent
public interface SimCardPrinterComponent {
    SimCardPrinter printer();

    @Module(subcomponents = SimCardPrinterComponent.class)
    interface InstallationModule {
    }

    @Subcomponent.Factory
    interface Factory {
        SimCardPrinterComponent create(
                @BindsInstance final SimCardParser parser,
                @BindsInstance @SimCardPrinterOutputter final Outputter outputter
        );
    }
}
