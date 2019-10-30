package ro.razvan.uniud.oop.telephoneOperator.model;

import org.jetbrains.annotations.NotNull;

public final class Person {
    @NotNull
    private final String name;
    @NotNull
    private final String surname;

    public Person(@NotNull String name, @NotNull String surname) {
        this.name = name;
        this.surname = surname;
    }

    @NotNull
    public String getName() {
        return name;
    }

    @NotNull
    public String getSurname() {
        return surname;
    }

    @Override
    public String toString() {
        return "Person{" +
                "name='" + name + '\'' +
                ", surname='" + surname + '\'' +
                '}';
    }
}
