/*
 * Classe SList<T>: Scheme-like Lists of T (generics)
 *
 * Definizione di una classe in Java per realizzare oggetti
 * assimilabili alle liste in Scheme, limitatamente al caso
 * di liste con elementi di tipo omogeneo.
 *
 * Le liste create sono "immutabili".
 *
 * Ultimo aggiornamento: 10/04/2018
 */


import java.util.function.*;


public class SList<T> {                    // Scheme-Like Lists of T


  // ----- Costante lista vuota: non puo' essere definita staticamente:
  
  // public static final SList<T> NULL_LIST = new SList<T>();
  
  
  // ----- Rappresentazione interna di una lista: private!
  
  // private final boolean empty;          // ridondante
  private final T first;                   // oggetti immutabili:
  private final SList<T> rest;             // variabili di istanza "final"
  
  
  // ----- Operazioni di base sulle liste, mutuate da Scheme
  
  public SList() {                         // creazione di una lista vuota
                                           // Scheme: null
    // empty = true;
    first = null;                          // valore irrilevante in questo caso
    rest = null;
  }
  
  public SList( T e, SList<T> tl ) {       // creazione di una lista non vuota:
                                           // Scheme: cons
    // empty = false;
    first = e;
    rest = tl;
  }
  
  
  public boolean isNull() {                // verifica se una lista e' vuota
                                           // Scheme: null?
    return ( first == null );              // empty
  }
  

  public T car() {                         // primo elemento di una lista
                                           // Scheme: car
    return first;                          // si assume: lista non vuota
  }
  
  
  public SList<T> cdr() {                  // resto di una lista
                                           // Scheme: cdr
    return rest;                           // si assume: lista non vuota
  }
  
  
  // ----- Realizzazione alternativa (sostanzialmente equivalente) del "cons"
  
  public SList<T> cons( T e ) {            // costruzione di nuove liste
                                           // Scheme: cons
    return new SList<T>( e, this );
  }
  
  
  // ----- Operazioni aggiuntive, definite in termini dei precedenti metodi
  
  public int length() {                    // lunghezza di una lista
                                           // Scheme: length
    if ( isNull() ) {
      return 0;
    } else {
      return ( 1 + cdr().length() );
    }
  }
  
  
  public T listRef( int k ) {              // elemento in posizione k
                                           // Scheme: list-ref
    if ( k == 0 ) {
      return car();
    } else {
      return ( cdr().listRef(k-1) );
    }
  }
  
  
  public boolean equals( SList<T> tl ) {   // contronto di liste
                                           // Scheme: equal?
    if ( isNull() || tl.isNull() ) {
      return ( isNull() && tl.isNull() );
    } else if ( car().equals(tl.car()) ) {
      return cdr().equals( tl.cdr() );
    } else {
      return false;
    }
  }
  
  public boolean equals( Object tl ) {     // metodo invocato in generale!
  
    return equals( (SList<T>) tl );
  }

  
  public SList<T> append( SList<T> tl ) {  // fusione di liste
                                           // Scheme: append
    if ( isNull() ) {
      return tl;
    } else {
      // return new SList<T>( car(), cdr().append(tl) );
      return ( cdr().append(tl) ).cons( car() );
    }
  }
  
  
  public SList<T> reverse() {              // rovesciamento di una lista
                                           // Scheme: reverse
    return reverseRec( new SList<T>() );
  }
  
  private SList<T> reverseRec( SList<T> re ) {
  
    if ( isNull() ) {                      // metodo di supporto: private!
      return re;
    } else {
      // return cdr().reverseRec( new SList<T>(car(),re) );
      return cdr().reverseRec( re.cons(car()) );
    }
  }
  
  
  // ----- Metodo con argomento procedurale - Scheme: map
  
  //       f : T -> Object (oggetti di qualunque tipo)
  //       lista restituita di tipo SList<Object>
  
  public SList<Object> map( Function<T,Object> f ) {
  
    if ( isNull() ) {
      return new SList<Object>();
    } else {
      // return new SList<Object>( f.apply(car()), cdr().map(f) );
      return ( cdr().map(f) ).cons( f.apply(car()) );
    }
  }
  
  
  // ----- Rappresentazione testuale (String) di una lista
  
  public String toString() {               // ridefinizione del metodo generale
                                           // per la visualizzazione testuale
    if ( isNull() ) {
      return "()";
    } else if ( cdr().isNull() ) {
      return "(" + car() + ")";
    } else {
      String rep = "(" + car();
      SList<T> r = cdr();
      while ( !r.isNull() ) {
        rep = rep + ", " + r.car();
        r = r.cdr();
      }
      return ( rep + ")" );
    }
  }


}  // class SList<T>

