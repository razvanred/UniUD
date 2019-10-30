package ro.razvan.uniud.oop.telephoneOperator.randomGenerator;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public final class RandomUniqueNumbersGenerator {

    private final @NotNull
    List<Integer> availableNumbers;
    private final int digits;

    RandomUniqueNumbersGenerator(final int digits) {
        this.digits = digits;
        availableNumbers = IntStream.range(0, (int) Math.pow(10D, digits))
                .boxed()
                .collect(Collectors.toCollection(ArrayList::new));

        Collections.shuffle(availableNumbers);
    }

    public int nextInt() {
        return availableNumbers.remove(0);
    }

    public @NotNull
    String nextString() {
        return String.format("%0" + digits + "d", nextInt());
    }
}
