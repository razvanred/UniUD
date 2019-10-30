package ro.razvan.uniud.oop.telephoneOperator.model;

import org.jetbrains.annotations.NotNull;

public final class PhoneCall {
    private final int timeInMinutes;
    private final @NotNull
    String destinationPhoneNumber;

    public PhoneCall(final int timeInMinutes, final @NotNull String destinationPhoneNumber) {
        this.timeInMinutes = timeInMinutes;
        this.destinationPhoneNumber = destinationPhoneNumber;
    }

    public int getTimeInMinutes() {
        return timeInMinutes;
    }

    public @NotNull
    String getDestinationPhoneNumber() {
        return destinationPhoneNumber;
    }

    @Override
    public String toString() {
        return "PhoneCall{" +
                "timeInMinutes=" + timeInMinutes +
                ", destinationPhoneNumber='" + destinationPhoneNumber + '\'' +
                '}';
    }
}
