import queens.ChessboardView;
public class Queens {
  private static final SList<Board> NULL_LIST=new SList<Board>();
  
  private static SList<Board> complete( Board b ) {
    int n = b.size();
    int q = b.queensOn();
    
    if ( q == n ) {
    
      return new SList<Board>(b, NULL_LIST);
    
    } else {
    
      int i = q + 1;
      SList<Board> list=NULL_LIST;
      
      for ( int j=1; j<=n; j=j+1 ) {
        if ( !b.underAttack(i,j) ) {
        
          list=list.append(complete( b.addQueen(i,j) ));
          
      }}
      return list;
    }
  }

  
  
  public static void main( String args[] ) {
  
    int n = Integer.parseInt( args[0] );
    ChessboardView gui = new ChessboardView( n );
    SList<Board> list=complete(new Board(n));
    while(!list.isNull()){
      System.out.println(list.car().positions());
      gui.setQueens( list.car().positions() );
      list=list.cdr();
    }
  }
  
  public static void test(int n){
    ChessboardView gui = new ChessboardView( n );
    SList<Board> list=complete(new Board(n));
    while(!list.isNull()){
      System.out.println(list.car().positions());
      gui.setQueens( list.car().positions() );
      list=list.cdr();
    }
  }
}  

