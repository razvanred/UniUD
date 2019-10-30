package ro.razvan.uniud.oop.telephoneOperator.randomGenerator;

import org.jetbrains.annotations.NotNull;
import org.junit.Test;

public class RandomUniqueIntGeneratorTest {

    private static final int TEST_DIGITS = 4;

    private final @NotNull
    RandomUniqueNumbersGenerator generator = new RandomUniqueNumbersGenerator(TEST_DIGITS);

    @Test
    public void testPhoneNumberRandom() {
        final var phone1 = generator.nextInt();
        final var phone2 = generator.nextInt();

        assert phone1 != phone2;
    }

    @Test
    public void testPhoneNumberRandomStringLength() {
        final var phone1 = generator.nextString();
        assert phone1.length() == TEST_DIGITS;
    }
}
