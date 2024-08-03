.PHONY final: Parser
	javac -cp ./libs/java-cup-11b.jar:./libs/jflex-full-1.9.1.jar:. *.java

.PHONY Parser: Lexer.java
	java -jar ./libs/java-cup-11b.jar -interface ./templates/parser.cup

Lexer.java:
	java -jar ./libs/jflex-full-1.9.1.jar --nobak -d ./ --skel ./skeleton.nested ./templates/lexer.jflex

demo:
	java -cp ./libs/java-cup-11b-runtime.jar:./libs/jflex-full-1.9.1.jar:. Main ./tests/demo1.txt ./tests/demo2.txt ./tests/demo3.txt ./tests/demo4.txt
