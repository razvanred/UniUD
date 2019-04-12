public class Board{
  private final int dim;
  private final int nq;
  private final IntSList row;
  private final IntSList col;
  private final IntSList ddx;
  private final IntSList dsx;
  
  public Board(int dim){
    this.dim=dim;
    this.nq=0;
    this.row=IntSList.NULL_INTLIST;
    this.col=IntSList.NULL_INTLIST;
    this.ddx=IntSList.NULL_INTLIST;
    this.dsx=IntSList.NULL_INTLIST;
  }
  
  private Board(int dim, int nq, IntSList row, IntSList col, IntSList ddx, IntSList dsx){
    this.dim=dim;
    this.nq=nq;
    this.row=row;
    this.col=col;
    this.ddx=ddx;
    this.dsx=dsx;
  }
  
  public int size(){
    return dim;
  }
  
  public int queensOn(){
    return nq;
  }
  
  public Board addQueen(int i, int j){
    return new Board(dim,
                     nq+1,
                     row.cons(i),
                     col.cons(j),
                     ddx.cons(i-j),
                     dsx.cons(i+j));
  }
  
  private static boolean contains(IntSList l, int n){
    if(l.isNull()){
      return false;
    } else {
        if(n==l.car()){
          return true;
        }
    }
    return contains(l.cdr(),n);
  }
  
  public boolean underAttack(int i, int j){
    return contains(row, i) || contains(col, j) || contains(ddx, i-j) || contains(dsx, i+j);
  }
  
  private static String notation(IntSList row, IntSList col){
    if(col.isNull()){
      return "";
    }
    return String.valueOf((char)(col.car()+96))+row.car()+" "+notation(row.cdr(), col.cdr());
  }
  
  public String arrangement(){
    return "< "+dim+", "+nq+", "+row.toString()+", "+col.toString()+", "+ddx.toString()+", "+dsx.toString()+", \" "+notation(row, col)+"\" >"; 
  }
}