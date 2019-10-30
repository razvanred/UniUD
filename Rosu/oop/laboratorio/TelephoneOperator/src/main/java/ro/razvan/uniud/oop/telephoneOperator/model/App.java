package ro.razvan.uniud.oop.telephoneOperator.model;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.di.AppComponent;
import ro.razvan.uniud.oop.telephoneOperator.di.TelephoneOperatorComponent;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.PrefixGenerator;
import ro.razvan.uniud.oop.telephoneOperator.randomGenerator.RandomUniqueNumbersGenerator;
import ro.razvan.uniud.oop.telephoneOperator.stdout.ErrorStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.OutputStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.Map;

public final class App {

    private final @NotNull
    Map<String, TelephoneOperator> operators;

    private final @NotNull
    RandomUniqueNumbersGenerator prefixGenerator;

    private final @NotNull
    Outputter outputStream;

    private final @NotNull
    Outputter errorStream;

    private final @NotNull
    TelephoneOperatorComponent.Factory telephoneOperatorComponentFactory;

    @Inject
    public App(
            @OutputStream final @NotNull Outputter outputStream,
            @ErrorStream final @NotNull Outputter errorStream,
            @PrefixGenerator final @NotNull RandomUniqueNumbersGenerator prefixGenerator,
            final @NotNull TelephoneOperatorComponent.Factory telephoneOperatorComponentFactory
    ) {
        operators = new HashMap<>();
        this.prefixGenerator = prefixGenerator;
        this.outputStream = outputStream;
        this.errorStream = errorStream;
        this.telephoneOperatorComponentFactory = telephoneOperatorComponentFactory;
    }

    public static void main(String[] args) {
        AppComponent.create().app();
    }

    @Inject
    public void execute() {
        operators.put("ho.", telephoneOperatorComponentFactory.create(prefixGenerator.nextString(), "ho").operator());
        outputStream.output(operators.get("ho.").createSim(new Person("Razvan", "Rosu")).toString());
        outputStream.output(operators.toString());
    }
}
