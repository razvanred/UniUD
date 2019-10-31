package ro.razvan.uniud.oop.telephoneOperator.randomGenerator.prefixGenerator;

import dagger.Module;
import dagger.Provides;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.prefixGenerator.PrefixGenerator;

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
