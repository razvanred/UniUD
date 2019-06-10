/* 
 * Codifica di Huffman (27/04/2016)
 *
 * Classe Huffman: algoritmi di compressione e decompressione
 */

import java.util.*;

import huffman_toolkit.*;


public class Huffman {


  private static final int CHARS = InputTextFile.CHARS;
  
  
  // ----------  Compressione  ----------
  
  
  // 1.
  // Istogramma delle frequenze dei caratteri
  // src: nome del documento da comprimere
  
  public static int[] charHistogram( String src ) {
  
    InputTextFile in = new InputTextFile( src );
    
    int[] freq = new int[ CHARS ];                          // istogramma delle frequenze dei caratteri
    
    for ( int c=0; c<CHARS; c=c+1 ) {                       // inizializzazione istogramma
      freq[c] = 0;
    }
    
    while ( in.textAvailable() ) {                          // scansione: frequenze dei caratteri
    
      int c = in.readChar();                                // (ulteriore) occorrenza di c
      freq[c] = freq[c] + 1;
    }
    in.close();
    
    return freq;
  }
  
  
  // 2.
  // Albero binario di codifica dei caratteri
  // freq: istogramma delle frequenze
  
  public static Node huffmanTree( int[] freq ) {
  
    PriorityQueue<Node> queue = new PriorityQueue<Node>();  // coda con priorita' di nodi
    
    for ( int c=0; c<CHARS; c=c+1 ) {                       // per ogni carattere utilizzato
      if ( freq[c] > 0 ) {
        Node n = new Node( (char) c, freq[c] );             // creazione del nodo corrispondente
        queue.add( n );                                     // e inserimento nella coda
    }}
    while ( queue.size() > 1 ) {                            // finche' c'e' piu' di un nodo nella coda
    
      Node l = queue.poll();                                // rimozione dei due nodi di peso minimo...
      Node r = queue.poll();
      
      Node n = new Node( l, r );                            // ...costruzione di un (sotto) albero
      queue.add( n );                                       // e inserimento nella coda
    }
    return queue.poll();                                    // radice dell'albero di Huffman
  }
  
  
  // 3.
  // Tabella di codifica dei caratteri:
  // root: nodo radice dell'albero di Huffman
  
  public static String[] huffmanCodesTable( Node root ) {
  
    String[] codes = new String[ CHARS ];                   // tabella dei codici di Huffman
    
    fillTable( root, "", codes );                           // compilazione della tabella
    
    return codes;
  }
  
  // Compilazione della tabella tramite visita ricorsiva dell'albero di Huffman
  // n:     nodo visitato
  // code:  codice binario del percorso dalla radice al nodo n
  // codes: variabile di stato per registrare i codici dei caratteri
  
  private static void fillTable( Node n, String code, String[] codes ) {
  
    if ( n.isLeaf() ) {
      codes[ n.character() ] = code;                        // nodo foglia: code = codice del carattere
    } else {
      fillTable( n.left(),  code+"0", codes );              // figlio sinistro: bit 0 nel codice
      fillTable( n.right(), code+"1", codes );              // figlio destro:   bit 1 nel codice
    }
  }
  
  
  // 4.
  // Codifica lineare dell'albero di Huffman tramite visita ricorsiva
  // n: nodo visitato
  // Struttura:
  // - una foglia e' codificata dal carattere che rappresenta
  //   (i caratteri speciali '@' e '\' sono preceduti da '\')
  // - un albero con piu' di un nodo e' codificato
  //   linearizzando i sottoalberi sinistro e destro,
  //   quindi giustapponendo il simbolo '@' e le stringhe ottenute
  
  public static String flattenTree( Node n ) {
    
    if ( n.isLeaf() ) {                                     // foglia: codifica del carattere
      char c = n.character();
      if ( (c == '\\') || (c == '@') ) {
        return ( "\\" + c );                                // caratteri speciali: \, @
      } else {
        return ( "" + c );                                  // altri caratteri
      }
    } else {
      return ( "@"                                          // @
             + flattenTree( n.left() )                      // codifica lineare del sottoalbero sinistro
             + flattenTree( n.right() )                     // codifica lineare del sottoalbero destro
               );
    }
  }
  
  
  // 5.
  // Compressione
  // src: nome del documento originale
  // dst: nome del documento compresso
  
  public static void compress( String src, String dst ) {
    
    // Scansione I : frequenze dei carattteri
    
    int[] freq = charHistogram( src );                      // istogramma delle frequenze
    
    Node root = huffmanTree( freq );                        // albero di Huffman
    
    int count = root.weight();                              // numero complessivo di caratteri
    String[] codes = huffmanCodesTable( root );             // tabella (array) dei codici
    
    // Scansione II : codifica di Huffman
    
    InputTextFile in = new InputTextFile( src );            // documento originale
    
    OutputTextFile out = new OutputTextFile( dst );         // documento compresso
    
    out.writeTextLine( "" + count );                        // intestazione del documento compresso
    out.writeTextLine( flattenTree(root) );
    
    for ( int j=0; j<count; j=j+1 ) {                       // scansione: codifica dei caratteri
    
      char c = in.readChar();
      out.writeCode( codes[c] );
    }
    in.close();
    out.close();
  }
  
  
  // ---------  Decompressione  ---------
  
  
  // 6.
  // Ricostruzione dell'albero di Huffman dalla sua codifica lineare
  // in: documento compresso
  // Struttura:
  // - una foglia e' rappresentata dal carattere corrispondente
  // - un albero con piu' di un nodo e' rappresentato
  //   dalla linearizzazione dei sottoalberi sinistro e destro,
  //   precedute dal simbolo '@'
  
  public static Node restoreTree( InputTextFile in ) {
  
    char c = (char) in.readChar();                          // carattere dell'intestazione
    
    if ( c == '@' ) {                                       // '@' ?
    
      Node left  = restoreTree( in );                       // sottoalbero sinistro
      
      Node right = restoreTree( in );                       // sottoalbero destro
      
      return new Node( left, right );                       // nodo genitore
    
    } else {
      if ( c == '\\' ) {
        c = (char) in.readChar();                           // carattere speciale (skip)
      }
      return new Node( c, 0 );                              // foglia
    }
  }
  
  
  // 7.
  // Decompressione
  // src: nome del documento compresso
  // dst: nome del documento ripristinato
  
  public static void decompress( String src, String dst ) {
    
    InputTextFile in = new InputTextFile( src );            // documento compresso
    
    int count = Integer.parseInt( in.readTextLine() );      // decodifica dell'intestazione
    Node root = restoreTree( in );
    
    String dummy = in.readTextLine();                       // codici caratteri dopo il fine-linea
    
    OutputTextFile out = new OutputTextFile( dst );         // documento ripristinato
    
    for ( int j=0; j<count; j=j+1 ) {                       // scansione: decodifica dei caratteri
      
      char c = decodeNextChar( root, in );
      out.writeChar( c );
    }
    in.close();
    out.close();
  }
  
  // Decodifica del carattere successivo
  // root: nodo radice dell'albero di Huffman
  // in:   documento compresso
  
  private static char decodeNextChar( Node root, InputTextFile in ) {
  
    Node n = root;                                          // percorso lungo l'albero di Huffman
    
    do {
      if ( in.readBit() == 0 ) {
        n = n.left();                                       // bit 0: figlio sinistro
      } else {
        n = n.right();                                      // bit 1: figlio destro
      }
    } while ( !n.isLeaf() );                                // fino al raggiungimento di una foglia
    
    return n.character();                                   // carattere individuato
  }
  
  
  // --------------  Main  --------------
  
  
  // 8.
  // Programma principale
  // Utilizzo:
  //   java -classpath "huffman_toolkit.jar:." Huffman compress   <documento originale> <documento compresso>
  //   java -classpath "huffman_toolkit.jar:." Huffman decompress <documento compresso> <documento ripristinato>
 
  public static void main( String[] args ) {
  
	  compress("C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1.txt", "C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1Compr.txt");
	  decompress("C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1Compr.txt", "C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1Dec.txt");
	  compressStd("C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1.txt", "C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1StdCompr.txt");
	  decompressStd("C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1StdCompr.txt", "C:\\Users\\grand\\eclipse-workspace\\Huffman3\\src\\doc1StdDec.txt");
    
  }
  
  
 //es. 1
  
  public static Node huffmanTreeStd(){
    
    PriorityQueue<Node> queue = new PriorityQueue<Node>();
    
    queue.add(new Node('a',8167));
    queue.add(new Node('b',1492));
    queue.add(new Node('c',2782));
    queue.add(new Node('d',4253));
    queue.add(new Node('e',12702));
    queue.add(new Node('f',2228));
    queue.add(new Node('g',2015));
    queue.add(new Node('h',6094));
    queue.add(new Node('i',6966));
    queue.add(new Node('j',0153));
    queue.add(new Node('k',0772));
    queue.add(new Node('l',4025));
    queue.add(new Node('m',2406));
    queue.add(new Node('n',6749));
    queue.add(new Node('o',7507));
    queue.add(new Node('p',1929));
    queue.add(new Node('q',95));
    queue.add(new Node('r',5987));
    queue.add(new Node('s',6327));
    queue.add(new Node('t',9056));
    queue.add(new Node('u',2758));
    queue.add(new Node('v',978));
    queue.add(new Node('w',2361));
    queue.add(new Node('x',0150));
    queue.add(new Node('y',1974));
    queue.add(new Node('z',0074));
    queue.add(new Node('A',8167/(24*5)));
    queue.add(new Node('B',1492/(24*5)));
    queue.add(new Node('C',2782/(24*5)));
    queue.add(new Node('D',4253/(24*5)));
    queue.add(new Node('E',12702/(24*5)));
    queue.add(new Node('F',2228/(24*5)));
    queue.add(new Node('G',2015/(24*5)));
    queue.add(new Node('H',6094/(24*5)));
    queue.add(new Node('I',6966/(24*5)));
    queue.add(new Node('J',0153/(24*5)));
    queue.add(new Node('K',0772/(24*5)));
    queue.add(new Node('L',4025/(24*5)));
    queue.add(new Node('M',2406/(24*5)));
    queue.add(new Node('N',6749/(24*5)));
    queue.add(new Node('O',7507/(24*5)));
    queue.add(new Node('P',1929/(24*5)));
    queue.add(new Node('Q',95/(24*5)));
    queue.add(new Node('R',5987/(24*5)));
    queue.add(new Node('S',6327/(24*5)));
    queue.add(new Node('T',9056/(24*5)));
    queue.add(new Node('U',2758/(24*5)));
    queue.add(new Node('V',978/(24*5)));
    queue.add(new Node('W',2361/(24*5)));
    queue.add(new Node('X',0150/(24*5)));
    queue.add(new Node('Y',1974/(24*5)));
    queue.add(new Node('Z',0074/(24*5)));
    queue.add(new Node('.',840));
    queue.add(new Node(',',1000));
    queue.add(new Node(' ',19767));
    queue.add(new Node('\n',322));
    queue.add(new Node('"',322));
    queue.add(new Node('1',322));
    queue.add(new Node('2',322));
    queue.add(new Node('3',322));
    queue.add(new Node('4',322));
    queue.add(new Node('5',322));
    queue.add(new Node('6',322));
    queue.add(new Node('7',322));
    queue.add(new Node('8',322));
    queue.add(new Node('9',322));
    queue.add(new Node('0',322));
    queue.add(new Node('!',1));
    queue.add(new Node('$',1));
    queue.add(new Node('%',1));
    queue.add(new Node('&',1));
    queue.add(new Node('/',1));
    queue.add(new Node('(',1));
    queue.add(new Node(')',1));
    queue.add(new Node('=',1));
    queue.add(new Node('?',1));
    queue.add(new Node('^',1));
    queue.add(new Node('\\',1));
    queue.add(new Node('\'',1));
    queue.add(new Node('<',1));
    queue.add(new Node('>',1));
    queue.add(new Node('+',1));
    queue.add(new Node('-',1));
    queue.add(new Node('*',1));
    queue.add(new Node('[',1));
    queue.add(new Node(']',1));
    queue.add(new Node('{',1));
    queue.add(new Node('}',1));
    queue.add(new Node('#',1));
    queue.add(new Node('@',1));
    queue.add(new Node('\t',1));
    queue.add(new Node('\r',322));
    queue.add(new Node('|',1));
    queue.add(new Node('_',1));
    queue.add(new Node(':',1));
    queue.add(new Node(';',1));
    queue.add(new Node('~',1));
    queue.add(new Node('`',1));
    
    while ( queue.size() > 1 ) {                            // finche' c'e' piu' di un nodo nella coda
    
      Node l = queue.poll();                                // rimozione dei due nodi di peso minimo...
      Node r = queue.poll();
      
      Node n = new Node( l, r );                            // ...costruzione di un (sotto) albero
      queue.add( n );                                       // e inserimento nella coda
    }
    return queue.poll();   
    
  }
  
  
  public static void compressStd( String src, String dst ) {
	    
	    // Scansione I : frequenze dei carattteri
	    
	    
	    Node root = huffmanTreeStd();
	    
	    int count = 0;                              // numero complessivo di caratteri
	    String[] codes = huffmanCodesTable( root );             // tabella (array) dei codici
	    
	    // Scansione II : codifica di Huffman
	    
	    InputTextFile in = new InputTextFile( src );            // documento originale
	    
	    OutputTextFile out = new OutputTextFile( dst );         // documento compresso
	    
	    while ( in.textAvailable() ) {                          
	        
	        int c = in.readChar();                               
	        count++;
	      }
	      
	      in.close();
	      in = new InputTextFile( src );       
	    
	    out.writeTextLine( "" + count );                        // intestazione del documento compresso
	    //out.writeTextLine( flattenTree(root) );
	    
	    for ( int j=0; j<count; j=j+1 ) {                       // scansione: codifica dei caratteri
	    
	      char c = in.readChar();
	      out.writeCode( codes[c] );
	    }
	    in.close();
	    out.close();
	  }
  
  
  
  public static void decompressStd( String src, String dst ) {
	    
	    InputTextFile in = new InputTextFile( src );            // documento compresso
	    
	    int count = Integer.parseInt( in.readTextLine() );      // decodifica dell'intestazione
	    Node root = huffmanTreeStd();
	    
	    OutputTextFile out = new OutputTextFile( dst );         // documento ripristinato
	    
	    for ( int j=0; j<count; j=j+1 ) {                       // scansione: decodifica dei caratteri
	      
	      char c = decodeNextChar( root, in );
	      out.writeChar( c );
	    }
	    in.close();
	    out.close();
	  }
  
  
  

}  // class Huffman


/*
 
Esempio:

  javac -classpath "huffman_toolkit.jar:." Huffman.java
  java  -classpath "huffman_toolkit.jar:." Huffman compress Huffman.java C.txt
  java  -classpath "huffman_toolkit.jar:." Huffman decompress C.txt D.txt
  diff Huffman.java D.txt

Fattore di compressione: circa 1/3 !

*/
