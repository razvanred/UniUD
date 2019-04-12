public class DynProg{
  
  private static final long UNKNOWN=0;
  
  public static long fib(int n){
    long[] h=new long[n+1];
    for(int i=0; i<=n; i++){
      h[i]=UNKNOWN;
    }
    return fibR(n, h);
  }
  
  private static long fibR(int n, long[] h){
    if(h[n]==UNKNOWN){
      if(n<2){
        h[n]= 1;
      } else {
        h[n]= fibR(n-1,h)+fibR(n-2,h);
      }
    }
    return h[n];
  }
  
  public static long fibSlow(int n){
    if(n<2){
      return 1;
    } else {
      return fibSlow(n-1)+fibSlow(n-2);
    }
  }
  
  //////////////////////////////
  
  public static long manh(int i, int j){
    long[][] h=new long[i+1][j+1];
    for(int x=0;x<=i;x++){
      for(int y=0;y<=j;y++){
        h[x][y]=UNKNOWN;
      }
    }
    return manhR(i,j,h);
  }
  
  public static long manh2(int i, int j){
    long[][] h=new long[i+1][j+1];
    for(int x=0;x<=i;x++){
      h[x][0]=1;
    }
    for(int y=1;y<=j;y++){
      h[0][y]=1;
    }
    for(int x=1;x<=i;x++){
      for(int y=1;y<=j;y++){
        h[x][y]=h[x-1][y]+h[x][y-1];
      }
    }
    return h[i][j];
  }
  
  private static long manhR(int i, int j, long[][] h){
    if(h[i][j]==UNKNOWN){
      if(i==0||j==0){
        h[i][j] = 1;
      } else {
        h[i][j] = manhR(i-1,j,h)+manhR(i,j-1,h);
      }
    }
    return h[i][j];
  }
  
  public static long manhSlow(int i, int j){
    if(i==0||j==0){
      return 1;
    } else {
      return manhSlow(i-1,j)+manhSlow(i,j-1);
    }
  }
}