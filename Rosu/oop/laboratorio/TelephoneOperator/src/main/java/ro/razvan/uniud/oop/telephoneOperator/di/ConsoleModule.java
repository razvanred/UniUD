package ro.razvan.uniud.oop.telephoneOperator.di;

import dagger.Module;
import dagger.Provides;
import ro.razvan.uniud.oop.telephoneOperator.stdout.ErrorStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.OutputStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

@Module
public interface ConsoleModule {

    @Provides
    static @ErrorStream
    Outputter error() {
        return System.err::println;
    }

    @Provides
    static @OutputStream
    Outputter output() {
        return System.out::println;
    }
}
