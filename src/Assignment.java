import jflex.base.Pair;

import java.util.Objects;
import java.util.Optional;

public class Assignment<T> {
    public final String lValue;
    public final Either<T, Pair<Optional<String>, String>> rValue;

    public Assignment(String lValue, Either<T, Pair<Optional<String>, String>> rValue) {
        this.lValue = lValue;
        this.rValue = rValue;
    }

    public boolean isReference() {
        return rValue.isRight();
    }

    public static Either<Void, Pair<Optional<String>, String>> dequalify(Either<?, Pair<Optional<String>, String>> rValue) {
        assert rValue.isRight();
        return Either.right(new Pair<>(Optional.empty(), rValue.getRight().snd));
    }

    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Section)) return false;

        Section section = (Section) o;
        return Objects.equals(lValue, section.name);
    }

    @Override
    public String toString() {
        return lValue + " = " + (isReference() ? "$" + ((rValue.getRight().fst.map(qualifier -> qualifier + ".").orElse("")) + rValue.getRight().snd) : rValue.getLeft().toString());
    }
}
