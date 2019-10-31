package ro.razvan.uniud.oop.telephoneOperator.util;

import org.jetbrains.annotations.NotNull;

public class Utilities {
    private Utilities(){
        throw new AssertionError();
    }

    @NotNull
    public static String toPriceString(final double amount) {
        return String.format("%.2f EUR", amount);
    }
}
