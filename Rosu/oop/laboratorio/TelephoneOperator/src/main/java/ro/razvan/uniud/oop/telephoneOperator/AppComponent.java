package ro.razvan.uniud.oop.telephoneOperator;

import dagger.Component;
import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.printer.SimCardPrinterComponent;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.prefixGenerator.PrefixGeneratorModule;
import ro.razvan.uniud.oop.telephoneOperator.stdout.console.ConsoleModule;
import ro.razvan.uniud.oop.telephoneOperator.stdout.fileOutput.FileOutputComponent;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperatorComponent;

import javax.inject.Singleton;

@Singleton
@Component(modules = {
        PrefixGeneratorModule.class,
        ConsoleModule.class,
        TelephoneOperatorComponent.InstallationModule.class,
        FileOutputComponent.InstallationModule.class,
        SimCardPrinterComponent.InstallationModule.class
})
public interface AppComponent {
    static @NotNull
    AppComponent create() {
        return DaggerAppComponent.create();
    }

    App app();
}
