package ro.razvan.uniud.oop.telephoneOperator.parser;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.simCard.SimCard;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperator;

import java.time.LocalDate;

import static ro.razvan.uniud.oop.telephoneOperator.util.Utilities.toPriceString;

public abstract class SimCardParser {

    @NotNull
    final String phoneNumber;

    @NotNull
    final String ownerNameSurname;

    @NotNull
    final String availableCredit;

    final int numberOfCalls;
    final boolean isSimPorted;
    final boolean isSimActive;

    @NotNull
    final LocalDate lastTopUp;

    @NotNull
    final String operatorName;

    SimCardParser(@NotNull final SimCard simCard, @NotNull final TelephoneOperator operator) {
        if (!simCard.operatorName().equals(operator.getName())) {
            throw new IllegalArgumentException("The telephone operator chosen is invalid.");
        }

        phoneNumber = simCard.getTelephoneNumber();
        final var owner = simCard.owner();
        ownerNameSurname = String.format("%s %s", owner.getName(), owner.surname());
        availableCredit = toPriceString(simCard.availableCreditInEuro());
        numberOfCalls = simCard.phoneCalls().length;
        isSimActive = simCard.isActive();
        isSimPorted = operator.isSimPorted(simCard);
        lastTopUp = simCard.lastTopUp();
        operatorName = simCard.operatorName();
    }

    @NotNull
    public abstract String parse();
}
