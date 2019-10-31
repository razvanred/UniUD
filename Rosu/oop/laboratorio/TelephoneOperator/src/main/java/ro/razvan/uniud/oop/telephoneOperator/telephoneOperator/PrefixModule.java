package ro.razvan.uniud.oop.telephoneOperator.telephoneOperator;

import dagger.Module;
import dagger.Provides;
import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.prefixGenerator.PrefixGenerator;

@Module
public interface PrefixModule {
    @Provides
    @NotNull
    @Prefix
    static String provideRandomPrefix(final @PrefixGenerator @NotNull RandomUniqueNumbersGenerator generator) {
        return generator.nextString();
    }
}
