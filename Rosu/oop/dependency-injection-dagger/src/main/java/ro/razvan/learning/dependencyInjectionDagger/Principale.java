package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.NotNull;

import java.util.Scanner;

public class Principale {

    public static void main(String[] args) {
        final @NotNull Scanner scanner = new Scanner(System.in);
        final @NotNull CommandProcessor processor = CommandProcessorFactory.create().processor();

        while (scanner.hasNextLine()) {
            processor.process(scanner.nextLine());
        }
    }
}
