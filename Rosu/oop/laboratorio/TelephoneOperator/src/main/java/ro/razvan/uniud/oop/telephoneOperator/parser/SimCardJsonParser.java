package ro.razvan.uniud.oop.telephoneOperator.parser;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.simCard.SimCard;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperator;
import ro.razvan.uniud.oop.telephoneOperator.util.JSONObjectBuilder;

public class SimCardJsonParser extends SimCardParser {

    public SimCardJsonParser(@NotNull final SimCard simCard, @NotNull final TelephoneOperator operator) {
        super(simCard, operator);
    }

    @Override
    public @NotNull
    String parse() {
        return new JSONObjectBuilder().put("phone_number", phoneNumber)
                .put("owner", ownerNameSurname)
                .put("available_credit", availableCredit)
                .put("number_of_calls", numberOfCalls)
                .put("is_ported", isSimPorted)
                .put("is_active", isSimActive)
                .put("last_top-up", lastTopUp)
                .put("operator", operatorName)
                .build()
                .toString();
    }
}
