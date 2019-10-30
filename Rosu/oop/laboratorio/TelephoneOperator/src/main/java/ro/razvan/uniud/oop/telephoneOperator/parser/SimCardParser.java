package ro.razvan.uniud.oop.telephoneOperator.parser;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.SimCard;
import ro.razvan.uniud.oop.telephoneOperator.model.TelephoneOperator;

import java.time.LocalDate;

public abstract class SimCardParser {

    @NotNull
    protected final String phoneNumber;
    @NotNull
    protected final String ownerNameSurname;
    @NotNull
    protected final String availableCredit;
    protected final int numberOfCalls;
    protected final boolean isSimPorted;
    protected final boolean isSimActive;
    @NotNull
    protected final LocalDate lastTopUp;

    public SimCardParser(@NotNull final SimCard simCard, @NotNull final TelephoneOperator operator) {
        phoneNumber = simCard.getTelephoneNumber();
        final var owner = simCard.getOwner();
        ownerNameSurname = String.format("%s %s", owner.getName(), owner.getSurname());
        availableCredit = String.format("%.02f", simCard.getAvailableCreditInEuro());
        numberOfCalls = simCard.getPhoneCalls().length;
        isSimActive = simCard.isActive();
        isSimPorted = operator.isSimPorted(simCard);
        lastTopUp = simCard.getLastTopUp();
    }

    @NotNull
    protected abstract String parse();
}
