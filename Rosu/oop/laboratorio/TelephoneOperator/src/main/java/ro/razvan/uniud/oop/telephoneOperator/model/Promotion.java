package ro.razvan.uniud.oop.telephoneOperator.model;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

public enum Promotion {
    UNLIMITED_MINUTES(10, "Unlimited Minutes"),
    CALL_AND_RECALL(15, "Call&Recall"),
    UNLIMITED_DATA(0, "Unlimited Data");

    private final double activationPriceInEuro;
    private final String name;

    Promotion(
            final double activationPriceInEuro,
            @NotNull final String name
    ) {
        this.activationPriceInEuro = activationPriceInEuro;
        this.name = name;
    }

    @Contract(pure = true)
    public double getActivationPriceInEuro() {
        return activationPriceInEuro;
    }

    @Override
    public String toString() {
        return "Promotion{" +
                "activationPriceInEuro=" + activationPriceInEuro +
                ", name='" + name + '\'' +
                '}';
    }
}

