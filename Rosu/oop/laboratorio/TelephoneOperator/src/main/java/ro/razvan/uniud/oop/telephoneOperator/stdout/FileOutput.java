package ro.razvan.uniud.oop.telephoneOperator.stdout;

import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public final class FileOutput implements Outputter {

    private final @NotNull
    String fileName;

    private final @NotNull
    Outputter errorOutputter;

    @Inject
    public FileOutput(
            final @NotNull String fileName,
            final @NotNull Outputter errorOutputter
    ) {
        this.fileName = fileName;
        this.errorOutputter = errorOutputter;
    }

    @Override
    public void output(final @NotNull String output) {
        try (final @NotNull BufferedWriter writer = new BufferedWriter(new FileWriter(fileName))) {
            writer.write(output);
        } catch (final @NotNull IOException exc) {
            errorOutputter.output(exc.getMessage());
        }
    }
}
