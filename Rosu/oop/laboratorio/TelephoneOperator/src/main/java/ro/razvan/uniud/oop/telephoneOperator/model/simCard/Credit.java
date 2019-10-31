package ro.razvan.uniud.oop.telephoneOperator.model.simCard;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import ro.razvan.uniud.oop.telephoneOperator.model.Promotion;

import java.time.LocalDate;

final class Credit {

    @Nullable
    private Promotion activePromotion;

    private double amount;

    @NotNull
    private LocalDate lastTopUp;

    Credit() {
        lastTopUp = LocalDate.now();
    }

    double availableAmountInEuro() {
        return amount;
    }

    @Nullable
    Promotion activePromotion() {
        return activePromotion;
    }

    @NotNull
    LocalDate lastTopUp() {
        return lastTopUp;
    }

    void topUpCredit(final double amount) {
        this.amount += amount;
        lastTopUp = LocalDate.now();
    }

    void setActivePromotion(final @NotNull Promotion promotion) {
        activePromotion = promotion;
    }

    void deactivatePromotion() {
        activePromotion = null;
    }
}
