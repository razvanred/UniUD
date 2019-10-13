public class Board{
  private final int dim;
  private final int nq;
  private final SList<Integer> row;
  private final SList<Integer> col;
  private final SList<Integer> ddx;
  private final SList<Integer> dsx;
  
  public Board(int dim){
    this.dim=dim;
    this.nq=0;
    this.row=new SList<Integer>();
    this.col=new SList<Integer>();
    this.ddx=new SList<Integer>();
    this.dsx=new SList<Integer>();
  }
  
  private Board(int dim, int nq, SList<Integer> row, SList<Integer> col, SList<Integer> ddx, SList<Integer> dsx){
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
  
  private static boolean contains(SList<Integer> l, int n){
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
  
  private static String notation(SList<Integer> row, SList<Integer> col){
    if(col.isNull()){
      return "";
    }
    return String.valueOf((char)(col.car()+96))+row.car()+" "+notation(row.cdr(), col.cdr());
  }
  
  public String arrangement(){
    return "< "+dim+", "+nq+", "+row.toString()+", "+col.toString()+", "+ddx.toString()+", "+dsx.toString()+", \" "+notation(row, col)+"\" >"; 
  }
}