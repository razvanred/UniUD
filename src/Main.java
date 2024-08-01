import java_cup.runtime.ComplexSymbolFactory;
import jflex.base.Pair;

import java.io.File;
import java.io.FileReader;
import java.util.Optional;

class Main {

    static public void main(String args[]) {
        try {
            ComplexSymbolFactory factory = new ComplexSymbolFactory();
            File input = new File(args[0]);
//            Lexer lexer = new Lexer(new FileReader(input),factory);
//            while(!lexer.yyatEOF()) {
//                Symbol token = lexer.next_token();
//                System.out.println(token.value);
//                System.out.println(token.toString());
//            }
            Lexer lexer = new Lexer(new FileReader(input),factory);
            Parser parser = new Parser(lexer,factory);
//            Lexer lexer2 = (Lexer) parser.getScanner();
//            SymbolFactory csf = lexer2.sf;

            Configuration configuration = (Configuration) parser.parse().value;
            if (!configuration.analyze()) {
                return;
            }

//            Section section = configuration.get("sez2");
//            Either<?, Pair<Optional<String>, String>> ass = section.assignments.get("var4").rValue;
            //Optional opt = configuration.getRvalueFromName("sez3", "var4");
            // System.out.println(configuration.resolveReference(section,ass).fst());
            //System.out.println(configuration.analyze());
            //configuration.get("sez3").assignments.remove("var3");
            //configuration.get("sez2").assignments.remove("var2");
//            configuration.removeBinding("sez3", "var3");
            //Triple<?,?,?> t = configuration.resolveReference(section,ass);
            //System.out.println(t.fst());
//            System.out.println();
//            configuration.prettyPrinter(lexer.annotatedComments);
//            System.out.println();
//            configuration.removeSection("sez2");
//            System.out.println(configuration);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
