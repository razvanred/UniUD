package ro.razvan.uniud.oop.telephoneOperator.di;

import dagger.BindsInstance;
import dagger.Module;
import dagger.Subcomponent;
import ro.razvan.uniud.oop.telephoneOperator.stdout.FileOutputModule;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Subcomponent(modules = {FileOutputModule.class})
public interface FileOutputComponent {
    Outputter fileOutput();

    @Module(subcomponents = FileOutputComponent.class)
    interface InstallationModule {
    }

    @Subcomponent.Factory
    interface Factory {
        FileOutputComponent create(@BindsInstance final String fileName);
    }
}
