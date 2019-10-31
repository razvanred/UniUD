package ro.razvan.uniud.oop.telephoneOperator.database;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import ro.razvan.uniud.oop.telephoneOperator.stdout.ErrorStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.OutputStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperator;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperatorComponent;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@Singleton
public final class InMemoryDatabase {

    @NotNull
    private final Outputter outputStream;

    @NotNull
    private final Outputter errorStream;

    @NotNull
    private final Map<String, TelephoneOperator> operators;

    @NotNull
    private final TelephoneOperatorComponent.Factory telephoneOperatorComponentFactory;

    @Inject
    public InMemoryDatabase(
            @OutputStream final @NotNull Outputter outputStream,
            @ErrorStream final @NotNull Outputter errorStream,
            final @NotNull TelephoneOperatorComponent.Factory telephoneOperatorComponentFactory
    ) {
        operators = new HashMap<>();
        this.outputStream = outputStream;
        this.errorStream = errorStream;
        this.telephoneOperatorComponentFactory = telephoneOperatorComponentFactory;
    }

    @NotNull
    public TelephoneOperator createOperator(final @NotNull String operatorName) {
        if (operators.containsKey(operatorName)) {
            throw new IllegalArgumentException("An operator with this name already exists");
        }
        return saveOperator(
                telephoneOperatorComponentFactory
                        .create(operatorName)
                        .operator()
        );
    }

    @NotNull
    private TelephoneOperator saveOperator(final @NotNull TelephoneOperator operator) {
        operators.put(operator.getName(), operator);
        return operator;
    }

    @NotNull
    public Optional<TelephoneOperator> findOperatorByName(final @NotNull String operatorName) {
        final @Nullable TelephoneOperator operator = operators.get(operatorName);
        if (operator == null) {
            errorStream.output("Operator not found :(");
            return Optional.empty();
        }

        return Optional.of(operator);
    }
}
