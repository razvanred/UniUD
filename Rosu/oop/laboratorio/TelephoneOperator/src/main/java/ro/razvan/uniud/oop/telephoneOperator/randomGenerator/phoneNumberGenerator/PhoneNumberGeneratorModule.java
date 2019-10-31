package ro.razvan.uniud.oop.telephoneOperator.randomGenerator.phoneNumberGenerator;

import dagger.Module;
import dagger.Provides;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperatorScope;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;

@Module
public interface PhoneNumberGeneratorModule {
    @TelephoneOperatorScope
    @Provides
    static @PhoneNumberGenerator
    RandomUniqueNumbersGenerator phoneNumberGenerator() {
        return new RandomUniqueNumbersGenerator(7);
    }
}
