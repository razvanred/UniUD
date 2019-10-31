package ro.razvan.uniud.oop.telephoneOperator.telephoneOperator;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.model.Person;
import ro.razvan.uniud.oop.telephoneOperator.model.Promotion;
import ro.razvan.uniud.oop.telephoneOperator.model.simCard.SimCard;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.phoneNumberGenerator.PhoneNumberGenerator;

import javax.inject.Inject;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public final class TelephoneOperator {

    private final @NotNull
    String prefix;

    private final @NotNull
    String name;

    private final @NotNull
    Map<String, SimCard> simCards;

    private final @NotNull
    RandomUniqueNumbersGenerator phoneNumberGenerator;

    @Inject
    public TelephoneOperator(
            @Prefix final @NotNull String prefix,
            @OperatorName final @NotNull String name,
            @PhoneNumberGenerator final @NotNull RandomUniqueNumbersGenerator phoneNumberGenerator
    ) {
        this.name = name;
        this.prefix = prefix;
        this.phoneNumberGenerator = phoneNumberGenerator;
        simCards = new LinkedHashMap<>();
    }

    @NotNull
    public SimCard createSim(
            @NotNull final Person owner
    ) {
        return cacheSimCard(
                new SimCard.Builder(owner, prefix, phoneNumberGenerator.nextString(), name)
                        .topUpCredit(10)
                        .build()
        );
    }

    @NotNull
    public SimCard createSim(
            @NotNull final Person owner,
            @NotNull final Promotion promotion
    ) {
        return cacheSimCard(
                new SimCard.Builder(owner, prefix, phoneNumberGenerator.nextString(), name)
                        .topUpCredit(10)
                        .activePromotion(promotion)
                        .build()
        );
    }

    @NotNull
    public SimCard portSim(
            @NotNull final SimCard oldSimCard
    ) {
        return cacheSimCard(
                new SimCard.Builder(oldSimCard.owner(), oldSimCard.getPrefix(), oldSimCard.getTelephoneNumberWithoutOperatorPrefix(), name)
                        .activePromotion(oldSimCard.getActivePromotion())
                        .addPhoneCalls(oldSimCard.phoneCalls())
                        .topUpCredit(oldSimCard.availableCreditInEuro())
                        .build()
        );
    }

    @NotNull
    @Contract("_ -> param1")
    private SimCard cacheSimCard(final SimCard simCard) {
        simCards.put(simCard.getTelephoneNumber(), simCard);
        return simCard;
    }

    public boolean removeSimCard(final SimCard simCard) {
        return simCards.remove(simCard.getTelephoneNumber(), simCard);
    }

    @NotNull
    public Optional<SimCard> getSimCard(final String telephoneNumber) {
        return Optional.ofNullable(simCards.get(telephoneNumber));
    }

    public boolean isSimPorted(@NotNull final SimCard simCard) {
        if (!simCard.operatorName().equals(name)) {
            throw new IllegalArgumentException("The given SIM is from another operator!");
        }
        return simCard.getPrefix().compareTo(prefix) != 0;
    }

    @Contract(pure = true)
    @NotNull
    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return "TelephoneOperator{" +
                "prefix='" + prefix + '\'' +
                ", name='" + name + '\'' +
                ", simCards=" + simCards +
                '}';
    }
}
