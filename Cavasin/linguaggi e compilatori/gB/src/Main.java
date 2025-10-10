import java_cup.runtime.ComplexSymbolFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.nio.file.Path;
import java.nio.file.Paths;

class Main {

    static public void main(String args[]) {

        ComplexSymbolFactory factory = new ComplexSymbolFactory();

        if (args.length < 1) {
            System.err.println("Missing file argument");
        }

        for (String arg : args) {
            Path path = Paths.get(arg);
            System.out.println();
            System.out.println(path);
            System.out.println("----------------");
            Lexer lexer = null;
            try {
                lexer = new Lexer(new FileReader(path.toFile()), factory, new Preprocessor(path.getParent()));
            } catch (FileNotFoundException ex) {
                System.err.println("File " + path + " not found");
                continue;
            }
            Parser parser = new Parser(lexer, factory);
            Configuration configuration = null;
            try {
                configuration = (Configuration) parser.parse().value;
            } catch (IllegalArgumentException ex) {
                System.out.println("Lexing failed: " + ex.getLocalizedMessage());
                continue;
            } catch (Exception ex) {
                System.out.println("Parsing failed: " + ex.getLocalizedMessage());
                continue;
            }
            if (!configuration.validate()) {
                continue;
            }

            if (path.getFileName().toString().equals("demo4.txt")) {
                System.out.println("var5a in sez5 has value: " + configuration.getRValueFromName("sez5", "var5a"));
                System.out.println("var5b in sez5 has value: " + configuration.getRValueFromName("sez5", "var5b"));
            }

            configuration.prettyPrint(lexer.annotatedComments);
        }
    }
}
