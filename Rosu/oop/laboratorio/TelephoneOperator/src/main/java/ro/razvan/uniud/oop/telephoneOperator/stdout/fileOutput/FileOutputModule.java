package ro.razvan.uniud.oop.telephoneOperator.stdout.fileOutput;

import dagger.Binds;
import dagger.Module;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Module
public interface FileOutputModule {
    @Binds
    Outputter bindFileOutput(final FileOutput fileOutput);
}
