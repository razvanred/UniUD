package ro.razvan.uniud.oop.telephoneOperator.model;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static java.time.temporal.ChronoUnit.YEARS;

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
    private double availableCreditInEuro;
    @Nullable
    private Promotion activePromotion;
    @NotNull
    private LocalDate lastTopUp;

    private SimCard(
            @NotNull final Builder builder
    ) {
        this.prefix = builder.prefix;
        this.telephoneNumber = builder.telephoneNumber;
        this.pukCode = builder.pukCode;
        this.availableCreditInEuro = builder.availableCreditInEuro;
        this.phoneCalls = builder.phoneCalls;
        this.owner = builder.owner;
        this.activePromotion = builder.activePromotion;
        this.lastTopUp = builder.lastTopUp;
    }

    @Contract(pure = true)
    @Nullable
    public Promotion getActivePromotion() {
        return activePromotion;
    }

    public void setActivePromotion(@NotNull final Promotion promotion) {
        activePromotion = promotion;
    }

    @Contract(pure = true)
    @NotNull
    public String getTelephoneNumber() {
        return prefix + telephoneNumber;
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
    public double getAvailableCreditInEuro() {
        return availableCreditInEuro;
    }

    public void setAvailableCreditInEuro(final double availableCreditInEuro) {
        this.availableCreditInEuro = availableCreditInEuro;
    }

    /**
     * Convertendo la lista in un array normale evito attacchi esterni alla classe non autorizzati
     *
     * @return array contenente la lista delle chiamate effettuate
     */
    @NotNull
    public PhoneCall[] getPhoneCalls() {
        return phoneCalls.toArray(PhoneCall[]::new);
    }

    @Contract(pure = true)
    @NotNull
    public Person getOwner() {
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
        activePromotion = null;
    }

    public void topUpCredit(final double credit) {
        availableCreditInEuro += credit;
        lastTopUp = LocalDate.now();
    }

    @NotNull
    @Contract(pure = true)
    @Override
    public String toString() {
        return "SimCard{" +
                "prefix='" + prefix + '\'' +
                ", telephoneNumber='" + telephoneNumber + '\'' +
                ", pukCode='" + pukCode + '\'' +
                ", availableCreditInEuro=" + availableCreditInEuro +
                ", phoneCalls=" + phoneCalls +
                ", owner='" + owner + '\'' +
                ", activePromotion=" + (activePromotion == null ? "no one" : activePromotion) +
                ", lastTopUp=" + lastTopUp +
                '}';
    }

    public boolean isActive() {
        return YEARS.between(lastTopUp, LocalDate.now()) < 1;
    }

    @NotNull
    public LocalDate getLastTopUp() {
        return lastTopUp;
    }

    public final static class Builder {
        @NotNull
        private final String prefix;
        @NotNull
        private final String telephoneNumber;
        @NotNull
        private final List<PhoneCall> phoneCalls;
        @NotNull
        private final Person owner;
        @NotNull
        private String pukCode;
        private double availableCreditInEuro;
        @Nullable
        private Promotion activePromotion;
        @NotNull
        private LocalDate lastTopUp;

        public Builder(
                @NotNull final Person owner,
                @NotNull final String prefix,
                @NotNull final String telephoneNumber
        ) {
            this.prefix = prefix;
            this.telephoneNumber = telephoneNumber;
            this.owner = owner;
            pukCode = String.format("%06d", ThreadLocalRandom.current().nextInt(9999));
            availableCreditInEuro = 0D;
            phoneCalls = new ArrayList<>();
            activePromotion = null;
            lastTopUp = LocalDate.now();
        }

        @Contract("_ -> this")
        public Builder topUpCredit(final double credit) {
            availableCreditInEuro += credit;
            lastTopUp = LocalDate.now();
            return this;
        }

        @Contract("_ -> this")
        public Builder activePromotion(@Nullable final Promotion activePromotion) {
            this.activePromotion = activePromotion;
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

