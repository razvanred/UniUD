package ro.razvan.uniud.oop.telephoneOperator.di;

import dagger.Component;
import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.App;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.PrefixGeneratorModule;

import javax.inject.Singleton;

@Singleton
@Component(modules = {
        PrefixGeneratorModule.class,
        ConsoleModule.class,
        TelephoneOperatorComponent.InstallationModule.class
})
public interface AppComponent {
    static @NotNull
    AppComponent create() {
        return DaggerAppComponent.create();
    }

    App app();
}
