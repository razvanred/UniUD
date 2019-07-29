import huffman_toolkit.*;
import java.lang.Math;

public class Main {
  
  public static void main(String[] args) {
    generateInput(args[0]);
//    compileTable(args[0],args[1]);

    Huffman.compress(args[0],"small_"+args[1]);
    Huffman.decompress("small_"+args[1],"original_"+args[0]);
  }
  

  // dst: nome del documento generato
  
  public static void generateInput(String dst){
    OutputTextFile out = new OutputTextFile( dst );
    String s="";
    
    for (int i=100/*1000+(int)Math.round(1000*Math.random())*/; i>0; i-- ) {
        s+=(char)Math.round(127*Math.random());
    }
    out.writeTextLine(s);
    out.close();
  }
  
  public static void compileTable( String src, String dst ) {
    // Scansione I : frequenze dei caratteri
    
    int[] freq = Huffman.charHistogram( src );                                   // istogramma delle frequenze
    String[] codes = Huffman.huffmanCodesTable(Huffman.huffmanTree( freq ));     // tabella (array) dei codici
    OutputTextFile out = new OutputTextFile( dst );                              // documento compresso
    
    String s;
    for (int i=0; i<freq.length; i=i+1 ) {                                       // scansione: codifica dei caratteri
      if(freq[i]>0){
        s=""+i+"\t";
        switch(i){
          case '\n':
            s+="\\n";
            break;
          case '\t':
            s+="\\t";
            break;
          case '\r':
            s+="\\r";
            break;
          default:
            s+=(char)i;
        }
        s+="\t"+freq[i]+"\t"+codes[i]+"\t"+codes[i].length();
        out.writeTextLine(s);
      }
    }
    out.close();
  }
}
