import java_cup.runtime.SymbolFactory;
import java.io.StringReader;
import java.io.Reader;
import java.io.File;
import java.io.FileReader;
import jflex.base.Pair;
import java.util.Optional;
import java.util.Map;
import java.util.LinkedHashMap;
%%

%public
%class Lexer
%unicode
%cupsym ParserSym
%cup
%line
%scanerror java.lang.IllegalArgumentException

/*
%eofval{
    return sf.newSymbol("EOF", ParserSym.EOF);
%eofval}
*/

LineTerminator = \R
WhiteSpace = \s
Word =  [:letter:]\w*
Number = \d+

Dots = \. | \.\.
Filename = (\w | \.)+

%{
    private SymbolFactory sf;
    private Preprocessor preprocessor;

    public final AnnotatedComments annotatedComments = new AnnotatedComments();

    public Lexer(Reader input, SymbolFactory sf, Preprocessor preprocessor) {
        this(input);
        this.sf = sf;
        this.preprocessor = preprocessor;
    }

    public void parsedSection(String section){
         annotatedComments.parsedSection(section);
    }

    public void parsedAssignment(String lValue){
        annotatedComments.parsedAssignment(lValue);
    }
%}

%%

"import" {WhiteSpace}* "<" \/? ( {Filename} | {Dots} \/ {Filename} | ( ({Dots} | {Filename} \/)+ {Filename} )) ">" {
                                // System.out.println(yytext().substring(8,yylength()-1));
                                int i = yytext().indexOf("<");
                                Optional<FileReader> fileopt = preprocessor.process(yytext().substring(i+1,yylength()-1));
                                if (fileopt.isPresent()) {
                                    yypushStream(fileopt.get());
                                }}
"="                      { return sf.newSymbol("EQUAL", ParserSym.EQUAL, yytext()); }
"false" | "true"         { return sf.newSymbol("BOOL", ParserSym.BOOL, Boolean.valueOf(yytext())); }
"inherit"                { return sf.newSymbol("INHERIT", ParserSym.INHERIT, yytext()); }
\[{Word}\]               {  String s = yytext().substring(1,yylength()-1);
                            annotatedComments.lexedSection(s);
                            return sf.newSymbol("SECTION", ParserSym.SECTION, s); }
{Word}                   { return sf.newSymbol("IDENTIFIER", ParserSym.IDENTIFIER, yytext()); }
\$({Word}\.)?{Word}      { final String[] t = yytext().substring(1).split("\\.", 2);
                           if(t.length == 2){
                               return sf.newSymbol("REFERENCE", ParserSym.REFERENCE, new Pair(Optional.of(t[0]), t[1]));
                           }
                           return sf.newSymbol("REFERENCE", ParserSym.REFERENCE, new Pair(Optional.empty(), t[0])); }
"\"" ~"\""               { return sf.newSymbol("STRING", ParserSym.STRING, yytext()); }
//"<" ~">"                 { return sf.newSymbol("PATH", ParserSym.PATH, yytext()); }
{Number}                 { return sf.newSymbol("NUMBER", ParserSym.NUMBER, Integer.valueOf(yytext())); }
"#"~{LineTerminator}       { annotatedComments.push(yytext()); }
<<EOF>>                  { if(yymoreStreams()){
                            yypopStream();
                           }else{
                             return sf.newSymbol("EOF", ParserSym.EOF);
                           } }
{WhiteSpace}             { /* ignore */ }
