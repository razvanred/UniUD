public class Board {
  private int size, queens;
  private SList<Integer> cols, rows, dright, dleft;
  
  public Board(int size) {
    this.size=size;
    queens=0;
    cols=new SList<Integer>();
    rows=new SList<Integer>();
    dright=new SList<Integer>();
    dleft=new SList<Integer>();
  }
  
  private Board(int size,int queens,SList<Integer> cols,SList<Integer> rows,SList<Integer> dright, SList<Integer> dleft){
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
  
  private boolean contains(SList<Integer> list,int n){
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
  
  public String representation(){
    String s="";
    SList<Integer> curCols=cols, curRows=rows;
    while(!curCols.isNull()){
      s=s+((char)(curCols.car()+96))+curRows.car()+" ";
      curCols=curCols.cdr();
      curRows=curRows.cdr();
    }
    return s;
  }
  
  public String toString(){ // String codifica testuale della configurazione 
    String s="< "+size+", "+queens+", "+rows+", "+cols+", "+dright+", "+dleft+", \" ";
    SList<Integer> curCols=cols, curRows=rows;
    while(!curCols.isNull()){
      s=s+((char)(curCols.car()+96))+curRows.car()+" ";
      curCols=curCols.cdr();
      curRows=curRows.cdr();
    }
    return s+"\">";
  }
  
}
