package ro.razvan.uniud.oop.telephoneOperator.stdout;

import dagger.Module;
import dagger.Provides;

@Module
public interface FileOutputModule {
    @Provides
    static Outputter provideFileOutput(
            final String fileName,
            final @ErrorStream Outputter errorStream
    ) {
        return new FileOutput(fileName, errorStream);
    }
}
