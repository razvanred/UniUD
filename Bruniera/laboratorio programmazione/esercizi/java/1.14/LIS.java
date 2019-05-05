import java.util.Arrays;

public class LIS{
  // costanti ed esempi
  private static final int UNKNOWN=-1;
  private static final SList<Integer> NULL_LIST=new SList<Integer>();
  
  public static int llisSlow( int[] s ) { // s[i] > 0 per i in [0,n-1], dove n = s.length
    return llisSlowRec( s, 0, 0 );
  }
  
  private static int llisSlowRec( int[] s, int i, int t ) {
    final int n = s.length;
    if ( i == n ) {       // il vettore è finito la lunghezza e 0
      return 0;
    } else if ( s[i] <= t ) {  // se il minorante è maggiore
      return llisSlowRec( s, i+1, t );   // salto il valore
    } else {
      return Math.max( 1+llisSlowRec(s,i+1,s[i]), llisSlowRec(s,i+1,t) ); // altrimenti prova saltando e non saltando
    }
  } 
  
  public static void testSlow(){
    System.out.println(llisSlow( new int[] {5, 4, 3, 2, 1} )); 
    System.out.println(llisSlow( new int[] {47, 38, 39, 25, 44} ));
    System.out.println(llisSlow( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} )); 
    System.out.println(llisSlow( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} )); 
    System.out.println(llisSlow( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} )); 
    System.out.println(llisSlow( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71}));
  }
  //////////////////////////////////////////////////////////////////
  // Parte 1
  
  private static int shiftIndex(int[] s, int i){ // accesso shiftato ai minoranti
    int thr;
    if(i==0){  // se l'indice � 0 restituisce 0
      thr=0;
    } else {   // altrimenti la cella precedente
      thr=s[i-1];
    }
    return thr;
  }
  
  public static int llis( int[] s ) { // funzione di chiamata
    int n=s.length;
    int h[][]= new int[n+1][n+1];   // inizializzo un vettore (n+1)X(n+1) a UNKNOWN 
    for(int i=0;i<=n;i++){
      for(int j=0;j<=n;j++){
        h[i][j]=UNKNOWN;
      }
    }
    return llisRec( s, 0, 0, h);    // chiamo la funzione ricorsiva
  }
  
  
  private static int llisRec( int[] s, int i, int t, int[][] h ) {  // funzione ricorsiva con memoization
    final int n = s.length;
    if(h[i][t]==UNKNOWN){   // se non ho memorizzato il valore lo calcolo
      if ( i == n ) {
        h[i][t] = 0;        // zero quando il vettore � finito
      } else {
        if ( s[i] <= shiftIndex(s, t) ) {
          h[i][t] = llisRec( s, i+1, t, h );    // se il minorante � maggiore del valore nel vetore passa alla cella successiva senza cambiare minorante ne quantit�
        } else {                                // altrimenti magiore tra:
          h[i][t] = Math.max(1+llisRec(s,i+1,i+1,h),    // 1+ il valore � nuovo minorante, passo alla successiva
                             llisRec(s,i+1,t,h) );      // 0+ stesso minorante, cella successiva
        }
      }
    }
    return h[i][t];         // restituisco il valore della cella in esame
  } 
  
  public static void test(){
    System.out.println(llis( new int[] {5, 4, 3, 2, 1} )); 
    System.out.println(llis( new int[] {47, 38, 39, 25, 44} ));
    System.out.println(llis( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} )); 
    System.out.println(llis( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} )); 
    System.out.println(llis( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} )); 
    System.out.println(llis( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71}));
  }
  
  //////// Bottom-Up

  public static int llisBU( int[] s ) {  // funzione llis con programmazione Bottom-Up
    int n=s.length;
    int h[][]= new int[n+1][n+1]; // vettore (n+1)X(n+1), inizializzo solo 0 i casi base [n][t]:0<=t<=n a 0
    for(int t=0;t<=n;t++){
      h[n][t]=0;
    }
    
    for(int i=n-1;i>=0;i--){   // salto l'ultima riga e parto dal basso
      for(int t=i;t>=0;t--){   // parto dalla cella [i][i] e scorro indietro fino a [i][0]
        if(s[i] <= shiftIndex(s, t)){   // se il minorante � maggiore
          h[i][t] = h[i+1][t];            // prendo il valore della cella sotto
        } else {                        // altrimenti maggiore tra:
          h[i][t] = Math.max(1 + h[i+1][i+1],   // 1+ valore memorizzato pi� a destra della riga sotto (valore calcolato con questo come minorante)
                             h[i+1][t]);        // 0+ valore della cella sotto                         (valore calcolato con lo stesso minorante)
        }
      }
    }
    return h[0][0];            // resitituisco la prima cella
  }
  
  public static void testBU(){
    System.out.println(llisBU( new int[] {5, 4, 3, 2, 1} )); 
    System.out.println(llisBU( new int[] {47, 38, 39, 25, 44} ));
    System.out.println(llisBU( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} )); 
    System.out.println(llisBU( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} )); 
    System.out.println(llisBU( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} )); 
    System.out.println(llisBU( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71}));
  }
  
  
  /////////////////////////////////////////////////////////////////////
  // parte 2
  
  public static SList<Integer> lis( int[] s ){  // funzione LIS con programmazione bottom up
    SList<Integer> list=NULL_LIST; //lista vuota per il risultato
    int n=s.length;
    int h[][]= new int[n+1][n+1]; // vettore (n+1)X(n+1), inizializzo a 0 i casi base [n][t]:0<=t<=n
    for(int t=0;t<=n;t++){
      h[n][t]=0;
    }
    
    for(int i=n-1;i>=0;i--){   // salto l'ultima riga e parto dal basso
      for(int t=i;t>=0;t--){   // parto dalla cella [i][i] e scorro indietro fino a [i][0]
        if(s[i] <= shiftIndex(s, t)){   // se il minorante � maggiore
          h[i][t] = h[i+1][t];            // prendo il valore della cella sotto
        } else {                        // altrimenti maggiore tra:
          h[i][t] = Math.max(1 + h[i+1][i+1],   // 1+ valore memorizzato pi� a destra della riga sotto (valore calcolato con questo come minorante)
                             h[i+1][t]);        // 0+ valore della cella sotto                         (valore calcolato con lo stesso minorante)
        }
      }
    }
    ////////// llisBU
    
    int i=0;  // parto dalla cella in cima [0][0]
    int t=0;
    
    while(h[i][t]!=0){  // finche esiste una sottosequenza di almeno 1 valore
      if(s[i] <= shiftIndex(s, t)){  // se il minorante � maggiore salto il valore
        i++;
      } else if(h[i+1][t]>h[i+1][i+1]){  // se il valore nella cella in basso � maggiore a quello nella cella in basso a destra (quindi [i+1][t]>=1+[i+1][i+1]):
        i++;                                // salta la cella
      } else {                           // altrimenti aggiungi in testa alla lista e passa alla cella in basso a destra
        list=list.cons(s[i]);
        i++;
        t=i;
      }
    }
    
    return list.reverse();            // resitituisco la sottosequenza in ordine crescente
  }
  
  public static void testLIS(){
    System.out.println(lis( new int[] {5, 4, 3, 2, 1} )); 
    System.out.println(lis( new int[] {47, 38, 39, 25, 44} ));
    System.out.println(lis( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} )); 
    System.out.println(lis( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} )); 
    System.out.println(lis( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} )); 
    System.out.println(lis( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71}));
  }
  /////////////////////////////////////////////////////////////////////
  // parte 3
  
  public static int[][] llisH( int[] s ) {
    int n=s.length;
    int h[][]= new int[n+1][n+1]; // vettore (n+1)X(n+1), inizializzo a 0 i casi base [n][t]:0<=t<=n
    for(int t=0;t<=n;t++){
      h[n][t]=0;
    }
    
    for(int i=n-1;i>=0;i--){   // salto l'ultima riga e parto dal basso
      for(int t=i;t>=0;t--){   // parto dalla cella [i][i] e scorro indietro fino a [i][0]
        if(s[i] <= shiftIndex(s, t)){   // se il minorante � maggiore
          h[i][t] = h[i+1][t];            // prendo il valore della cella sotto
        } else {                        // altrimenti maggiore tra:
          h[i][t] = Math.max(1 + h[i+1][i+1],   // 1+ valore memorizzato pi� a destra della riga sotto (valore calcolato con questo come minorante)
                             h[i+1][t]);        // 0+ valore della cella sotto                         (valore calcolato con lo stesso minorante)
        }
      }
    }
    
    
    return h;            // resitituisco il vettore
  }
  
  /*
   * vengono valorizzate solo met� delle celle, formando una sorta di triangolo di dati,
   * ad ogni riga [i] solo le celle delle colonne fino alla [t]:t=i, 
   * questo perch� il valore di cui il valore cella in valutazione deve essere maggiore
   * pu� essere solo tra quelli che appaiono prima nelll'array,
   * dal momento l'accesso al vettore avviene shiftandolo a destra quando cerco un "minorante"
   * e normalmente quando cerco il valore della cella, contando la Nesima cella come nulla
   * vale la regola 0<=t<=i<=n
   */
  
  public static void testH(){
    System.out.println(Arrays.deepToString(llisH( new int[] {5, 4, 3, 2, 1} ))); 
    System.out.println(Arrays.deepToString(llisH( new int[] {47, 38, 39, 25, 44} )));
    System.out.println(Arrays.deepToString(llisH( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} ))); 
    System.out.println(Arrays.deepToString(llisH( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} ))); 
    System.out.println(Arrays.deepToString(llisH( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} ))); 
    System.out.println(Arrays.deepToString(llisH( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71})));
  }
}