package ro.razvan.uniud.oop.telephoneOperator.randomGenerator;

import dagger.Module;
import dagger.Provides;
import ro.razvan.uniud.oop.telephoneOperator.di.TelephoneOperatorScope;

@Module
public interface PhoneNumberGeneratorModule {
    @TelephoneOperatorScope
    @Provides
    static @PhoneNumberGenerator
    RandomUniqueNumbersGenerator phoneNumberGenerator() {
        return new RandomUniqueNumbersGenerator(7);
    }
}
