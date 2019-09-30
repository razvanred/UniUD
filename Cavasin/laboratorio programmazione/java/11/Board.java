public class Board {
  private int size, queens;
  private IntSList cols, rows, dright, dleft;
  
  public Board(int size) {
    this.size=size;
    queens=0;
    cols=new IntSList();
    rows=new IntSList();
    dright=new IntSList();
    dleft=new IntSList();
  }
  
  private Board(int size,int queens,IntSList cols,IntSList rows,IntSList dright, IntSList dleft){
    this.size=size;
    this.queens=queens;
    this.cols=cols;
    this.rows=rows;
    this.dright=dright;
    this.dleft=dleft;
  }
  
  public int size(){ // dimensione della scacchiera
    return size;
  }
  
  public int queensOn (){ // int numero di regine collocate sulla scacchiera
    return queens;
  }
  
  public boolean underAttack(int i,int j){ //  la posizione di coordinate <i, j> è minacciata?
    return (contains(cols,i)||contains(rows,j)||contains(dright,i-j)||contains(dleft,i+j));
  }
  
  private boolean contains(IntSList list,int n){
    while(!list.isNull()){
      if(list.car()==n)
        return true;
      list=list.cdr();
    }
    return false;
  }
  
  public Board addQueen(int i,int j){ // nuova scacchiera con una regina in posizione <i, j> che si aggiunge alla configurazione di b
    return new Board(size,queens+1,cols.cons(i),rows.cons(j),dright.cons(i-j),dleft.cons(i+j));
  }
  
  public String arrangement(){ // String codifica testuale della configurazione 
    String s="< "+size+", "+queens+", "+rows+", "+cols+", "+dright+", "+dleft+", \" ";
    IntSList curCols=cols, curRows=rows;
    while(!curCols.isNull()){
      s=s+(char)(curCols.car()+96)+curRows.car()+" ";
      curCols=curCols.cdr();
      curRows=curRows.cdr();
    }
    return s+"\" >";
  }
  
}
