package ro.razvan.uniud.lis.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.Serializable;
import java.util.Objects;

/**
 * Classe ispirata a kotlin.Pair
 * Package-private class
 *
 * @param <T> tipo del primo valore
 * @param <S> tipo del secondo valore
 * @author Răzvan Roşu
 */
final class Pair<T, S> implements Serializable {

    @Nullable
    private final T first;

    @Nullable
    private final S second;

    Pair(@Nullable final T first, @Nullable final S second) {
        this.first = first;
        this.second = second;
    }

    @Nullable
    T getFirst() {
        return first;
    }

    @Nullable
    S getSecond() {
        return second;
    }

    @NotNull
    T getFirstNonNull() {
        return Objects.requireNonNull(first);
    }

    @NotNull
    S getSecondNonNull() {
        return Objects.requireNonNull(second);
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ")";
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Pair)) {
            return false;
        }

        final var pair = (Pair) obj;

        return pair.first == first && pair.second == second;
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second);
    }

}
