package ro.razvan.uniud.oop.telephoneOperator.stdout.fileOutput;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.stdout.ErrorStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;

import javax.inject.Inject;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public final class FileOutput implements Outputter {

    @NotNull
    private final String fileName;

    @NotNull
    private final Outputter errorStream;

    @Inject
    public FileOutput(
            @FileName final @NotNull String fileName,
            @ErrorStream final @NotNull Outputter errorStream
    ) {
        this.fileName = fileName;
        this.errorStream = errorStream;
    }

    @Override
    public void output(final @NotNull String output) {
        try (final @NotNull BufferedWriter writer = new BufferedWriter(new FileWriter(fileName))) {
            writer.write(output);
        } catch (final @NotNull IOException exc) {
            errorStream.output(exc.getMessage());
        }
    }
}
