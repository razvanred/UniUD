package ro.razvan.uniud.puzzleboard.java.util;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.Serializable;
import java.util.Objects;

/**
 * Classe ispirata a kotlin.Pair
 *
 * @param <F>
 * @param <S>
 */
public final class Pair<F, S> implements Serializable {

    @Nullable
    private final F first;

    @Nullable
    private final S second;

    public Pair(@Nullable final F first, @Nullable final S second) {
        this.first = first;
        this.second = second;
    }

    @Nullable
    public F getFirst() {
        return first;
    }

    @Nullable
    public S getSecond() {
        return second;
    }

    @NotNull
    public F getFirstNonNull() {
        return Objects.requireNonNull(first);
    }

    @NotNull
    public S getSecondNonNull() {
        return Objects.requireNonNull(second);
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ")";
    }

    @Override
    public boolean equals(@Nullable final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        final Pair<?, ?> pair = (Pair<?, ?>) o;

        if (!Objects.equals(first, pair.first)) return false;
        return Objects.equals(second, pair.second);
    }

    @Override
    public int hashCode() {
        int result = first != null ? first.hashCode() : 0;
        result = 31 * result + (second != null ? second.hashCode() : 0);
        return result;
    }
}
