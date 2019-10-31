package ro.razvan.uniud.oop.telephoneOperator.stdout.fileOutput;

import dagger.BindsInstance;
import dagger.Module;
import dagger.Subcomponent;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Subcomponent(modules = {FileOutputModule.class})
public interface FileOutputComponent {
    Outputter fileOutput();

    @Module(subcomponents = FileOutputComponent.class)
    interface InstallationModule {
    }

    @Subcomponent.Factory
    interface Factory {
        FileOutputComponent create(@BindsInstance @FileName final String fileName);
    }
}
