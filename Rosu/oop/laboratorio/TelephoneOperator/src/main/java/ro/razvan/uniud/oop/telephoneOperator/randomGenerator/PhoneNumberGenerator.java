package ro.razvan.uniud.oop.telephoneOperator.randomGenerator;

import javax.inject.Qualifier;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Qualifier
@Retention(RetentionPolicy.RUNTIME)
public @interface PhoneNumberGenerator {
}
