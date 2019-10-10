public class StringSList {
  // ----- Costante lista vuota (condivisa)
  
  public static final StringSList NULL_STRINGLIST = new StringSList();
  
  // ----- Rappresentazione interna di una lista: private!
  private final boolean empty;                         // oggetti immutabili:
  private final String first;                          // variabili di istanza "final"
  private final StringSList rest;
  
  // ----- Operazioni di base sulle liste, mutuate da Scheme
  public StringSList() {                               // creazione di una lista vuota
    // Scheme: null
    empty = true;
    first = "";                                        // valore irrilevante in questo caso
    rest = null;
  }
  
  public StringSList( String e, StringSList il ) {     // creazione di una lista non vuota:
    // Scheme: cons
    empty = false;
    first = e;
    rest = il;
  }
  
  public boolean isNull() {                            // verifica se una lista e' vuota
    // Scheme: null?
    return ( empty );
  }
  
  public String car() {                                // primo elemento di una lista
    // Scheme: car
    return first;                                      // si assume: lista non vuota
  }
  
  public StringSList cdr() {                           // resto di una lista
    // Scheme: cdr
    return rest;                                       // si assume: lista non vuota
  }
  
  public StringSList cons( String e ) {                // costruzione di nuove liste
    // Scheme: cons
    return new StringSList( e, this );
  }
  
  // ----- Operazioni aggiuntive, definite in termini dei precedenti metodi
  
  public int length() {                                // lunghezza di una lista
    // Scheme: length
    if ( isNull() ) {
      return 0;
    } else {
      return ( 1 + cdr().length() );
    }
  }
  
  public String listRef( int k ) {                     // elemento in posizione k
    // Scheme: list-ref
    if ( k == 0 ) {
      return car();
    } else {
      return ( cdr().listRef(k-1) );
    }
  }
  
  public boolean equals( StringSList il ) {            // contronto di liste
    // Scheme: equal?
    if ( isNull() || il.isNull() ) {
      return ( isNull() && il.isNull() );
    } else if ( car().equals(il.car()) ) {
      return cdr().equals( il.cdr() );
    } else {
      return false;
    }
  }
  
  public StringSList append( StringSList il ) {        // fusione di liste
    // Scheme: append
    if ( isNull() ) {
      return il;
    } else {
      // return new StringSList( car(), cdr().append(il) );
      return ( cdr().append(il) ).cons( car() );
    }
  }
  
  public StringSList reverse() {                       // rovesciamento di una lista
    // Scheme: reverse
    return reverseRec( new StringSList() );
  }
  
  private StringSList reverseRec( StringSList re ) {
    if ( isNull() ) {                                  // metodo di supporto: private!
      return re;
    } else {
      // return cdr().reverseRec( new StringSList(car(),re) );
      return cdr().reverseRec( re.cons(car()) );
    }
  }
  
  // ----- Rappresentazione testuale (String) di una lista
  public String toString() {                           // ridefinizione del metodo generale
    // per la visualizzazione testuale
    if ( empty ) {
      return "()";
    } else if ( rest.isNull() ) {
      return "(" + first + ")";
    } else {
      String rep = "(" + first;
      StringSList r = rest;
      while ( !r.isNull() ) {
        rep = rep + ", " + r.car();
        r = r.cdr();
      }
      return ( rep + ")" );
    }
  }
}  // class IntSList
