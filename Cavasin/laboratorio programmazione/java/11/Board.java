public class Board {
  int board[][], size, queens;
  IntSList rows, cols, dright, dleft;
  
  public Board(int n) {
    size=n;
    board=new int[size][size];
  }
  
  public int size(){ // dimensione della scacchiera
    return size;
  }  
  public queensOn (){ // int numero di regine collocate sulla scacchiera
    return queens;
  }
  public boolean underAattack(i,j){ //  la posizione di coordinate <i, j> è minacciata?
  }
  public Board addQueen(i,j){ // nuova scacchiera con una regina in posizione <i, j> che si aggiunge alla configurazione di b
    queens++;
    rows
  }
  public boolean arrangement(){ // String codifica testuale della configurazione 
  }
  
}
