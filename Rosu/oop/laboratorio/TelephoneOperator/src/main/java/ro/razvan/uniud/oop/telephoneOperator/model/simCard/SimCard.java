package ro.razvan.uniud.oop.telephoneOperator.model.simCard;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import ro.razvan.uniud.oop.telephoneOperator.model.Person;
import ro.razvan.uniud.oop.telephoneOperator.model.PhoneCall;
import ro.razvan.uniud.oop.telephoneOperator.model.Promotion;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static java.time.temporal.ChronoUnit.YEARS;
import static ro.razvan.uniud.oop.telephoneOperator.util.Utilities.toPriceString;

public final class SimCard {

    @NotNull
    private final String prefix;

    @NotNull
    private final String telephoneNumber;

    @NotNull
    private final String pukCode;

    @NotNull
    private final List<PhoneCall> phoneCalls;

    @NotNull
    private final Person owner;

    @NotNull
    private final Credit credit;

    @NotNull
    private final String operatorName;

    private SimCard(
            @NotNull final Builder builder
    ) {
        this.prefix = builder.prefix;
        this.telephoneNumber = builder.telephoneNumber;
        this.pukCode = builder.pukCode;
        this.credit = builder.credit;
        this.phoneCalls = builder.phoneCalls;
        this.owner = builder.owner;
        this.operatorName = builder.operatorName;
    }

    @Contract(pure = true)
    @Nullable
    public Promotion getActivePromotion() {
        return credit.activePromotion();
    }

    public void changeActivePromotion(@NotNull final Promotion promotion) {
        credit.setActivePromotion(promotion);
    }

    @Contract(pure = true)
    @NotNull
    public String getTelephoneNumber() {
        return prefix + telephoneNumber;
    }

    @Contract(pure = true)
    @NotNull
    public String getTelephoneNumberWithoutOperatorPrefix() {
        return telephoneNumber;
    }

    @Contract(pure = true)
    @NotNull
    public String getPrefix() {
        return prefix;
    }

    @Contract(pure = true)
    @NotNull
    public String getPukCode() {
        return pukCode;
    }

    @Contract(pure = true)
    public double availableCreditInEuro() {
        return credit.availableAmountInEuro();
    }

    /**
     * Convertendo la lista in un array normale evito attacchi esterni alla classe non autorizzati
     *
     * @return array contenente la lista delle chiamate effettuate
     */
    @NotNull
    public PhoneCall[] phoneCalls() {
        return phoneCalls.toArray(PhoneCall[]::new);
    }

    @Contract(pure = true)
    @NotNull
    public Person owner() {
        return owner;
    }

    public void addPhoneCall(@NotNull final PhoneCall... phoneCalls) {
        Collections.addAll(this.phoneCalls, phoneCalls);
    }

    public int getConversationTimeInMinutes() {
        return phoneCalls.stream()
                .mapToInt(PhoneCall::getTimeInMinutes)
                .sum();
    }

    @NotNull
    public PhoneCall[] getPhoneCallsByPhoneNumber(@NotNull final String phoneNumber) {
        return phoneCalls.stream()
                .filter((phoneCall) -> phoneCall.getDestinationPhoneNumber().compareTo(phoneNumber) == 0)
                .toArray(PhoneCall[]::new);
    }

    public void deactivatePromotion() {
        credit.deactivatePromotion();
    }

    public void topUpCredit(final double amount) {
        credit.topUpCredit(amount);
    }

    @NotNull
    @Contract(pure = true)
    @Override
    public String toString() {
        final @Nullable Promotion activePromotion = getActivePromotion();

        return "SimCard{" +
                "prefix='" + prefix + '\'' +
                ", telephoneNumber='" + getTelephoneNumber() + '\'' +
                ", pukCode='" + pukCode + '\'' +
                ", availableCreditInEuro=\'" + toPriceString(credit.availableAmountInEuro()) + '\'' +
                ", phoneCalls=" + phoneCalls +
                ", owner=" + owner +
                ", activePromotion='" + (activePromotion == null ? "no one" : activePromotion) + "'" +
                ", lastTopUp=" + credit.lastTopUp() +
                ", operatorName='" + operatorName + '\'' +
                '}';
    }

    public boolean isActive() {
        return YEARS.between(credit.lastTopUp(), LocalDate.now()) < 1;
    }

    @NotNull
    public String operatorName() {
        return operatorName;
    }

    @NotNull
    public LocalDate lastTopUp() {
        return credit.lastTopUp();
    }

    @NotNull
    public PhoneCall[] getPhoneCalls() {
        return phoneCalls.toArray(PhoneCall[]::new);
    }

    public final static class Builder {

        @NotNull
        private final String prefix;

        @NotNull
        private final String operatorName;

        @NotNull
        private final String telephoneNumber;

        @NotNull
        private final List<PhoneCall> phoneCalls;

        @NotNull
        private final Person owner;

        @NotNull
        private String pukCode;

        @NotNull
        private final Credit credit;

        public Builder(
                @NotNull final Person owner,
                @NotNull final String prefix,
                @NotNull final String telephoneNumber,
                @NotNull final String operatorName
        ) {
            this.prefix = prefix;
            this.telephoneNumber = telephoneNumber;
            this.owner = owner;
            this.operatorName = operatorName;
            pukCode = String.format("%06d", ThreadLocalRandom.current().nextInt(9999));
            phoneCalls = new ArrayList<>();
            credit = new Credit();
        }

        @Contract("_ -> this")
        public Builder topUpCredit(final double amount) {
            credit.topUpCredit(amount);
            return this;
        }

        @Contract("_ -> this")
        public Builder activePromotion(@Nullable final Promotion activePromotion) {
            if (activePromotion != null) {
                credit.setActivePromotion(activePromotion);
            } else {
                credit.deactivatePromotion();
            }
            return this;
        }

        @Contract("_ -> this")
        public Builder addPhoneCalls(@NotNull final PhoneCall... phoneCalls) {
            Collections.addAll(this.phoneCalls, phoneCalls);
            return this;
        }

        @Contract(value = " -> new", pure = true)
        @NotNull
        public SimCard build() {
            return new SimCard(this);
        }
    }
}

