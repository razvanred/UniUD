package ro.razvan.uniud.oop.telephoneOperator.parser;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.SimCard;
import ro.razvan.uniud.oop.telephoneOperator.model.TelephoneOperator;

public class SimCardTxtParser extends SimCardParser {

    public SimCardTxtParser(@NotNull final SimCard simCard, @NotNull final TelephoneOperator operator) {
        super(simCard, operator);
    }

    @Override
    protected @NotNull
    String parse() {
        return "Phone Number: " + phoneNumber + "\n" +
                "Owner: " + ownerNameSurname + "\n" +
                "Available Credit: " + availableCredit + "\n" +
                "Number of Calls:" + numberOfCalls + "\n" +
                "Ported SIM: " + humanTranslationBoolean(isSimPorted) + "\n" +
                "Active SIM: " + humanTranslationBoolean(isSimActive) + "\n" +
                "Last top-up: " + lastTopUp + "\n";
    }

    @Contract(pure = true)
    private @NotNull
    String humanTranslationBoolean(final boolean value) {
        return value ? "Yes" : "No";
    }
}

