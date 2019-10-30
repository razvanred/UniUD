package ro.razvan.uniud.oop.telephoneOperator.randomGenerator;

import dagger.Module;
import dagger.Provides;

import javax.inject.Singleton;

@Module
public interface PrefixGeneratorModule {
    @Provides
    @Singleton
    static @PrefixGenerator
    RandomUniqueNumbersGenerator prefixGenerator() {
        return new RandomUniqueNumbersGenerator(3);
    }
}
