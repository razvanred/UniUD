public class LCS {
  
  private static final int UNKNOWN=-1;
  
  public static int llcs(String u, String v){
    int i=u.length();
    int j=v.length();
    int[][] h=new int[i+1][j+1];
    
    for(int x=0;x<=i;x++){
      for(int y=0;y<=j;y++){
        h[x][y]=UNKNOWN;
      }
    }
    
    return llcsR(u,v,h);
  }
  
  private static int llcsR(String u, String v, int[][] h){
    int i=u.length();
    int j=v.length();
    
    if(h[i][j]==UNKNOWN){
      if(u.equals("") || v.equals("")){
        h[i][j] = 0;
      } else if(u.charAt(0)==v.charAt(0)){
        h[i][j] = 1+llcsR(u.substring(1),v.substring(1),h);
      } else {
        h[i][j] = Math.max(llcsR(u.substring(1),v,h),
                           llcsR(u,v.substring(1),h));
      }
    }
    
    return h[i][j];
  }
  
  public static int llcsSlow(String u, String v){
    if(u.equals("") || v.equals("")){
      return 0;
    } else if(u.charAt(0)==v.charAt(0)){
      return 1+llcs(u.substring(1),v.substring(1));
    } else {
      return Math.max(llcs(u.substring(1),v),
                      llcs(u,v.substring(1)));
    }
  }
  
  public static int llcsBU(String u, String v){
    int m=u.length();
    int n=v.length();
    int[][] h=new int[m+1][n+1];
    
    for(int y=0;y<=n;y++){
      h[0][y]=0;
    }
    
    for(int x=1;x<=m;x++){
       h[x][0]=0;
    }
    
    for(int x=1;x<=m;x++){
      for(int y=1;y<=n;y++){
        if(u.charAt(m-x)==v.charAt(n-y)){
          h[x][y] = 1+h[x-1][y-1];
        } else {
          h[x][y] = Math.max(h[x-1][y],h[x][y-1]);
        }
      }
    }
    
    return h[m][n];
  }
  
  public static String LCS(String u, String v){
    int m=u.length();
    int n=v.length();
    int[][] h=new int[m+1][n+1];
    
    for(int y=0;y<=n;y++){
      h[0][y]=0;
    }
    
    for(int x=1;x<=m;x++){
       h[x][0]=0;
    }
    
    for(int x=1;x<=m;x++){
      for(int y=1;y<=n;y++){
        if(u.charAt(m-x)==v.charAt(n-y)){
          h[x][y] = 1+h[x-1][y-1];
        } else {
          h[x][y] = Math.max(h[x-1][y],h[x][y-1]);
        }
      }
    }
    /////////llcsBU
    
    int i=m;
    int j=n;
    String s="";
    
    while(h[i][j]!=0){
      if(u.charAt(m-i)==v.charAt(n-j)){
        s+=u.charAt(m-i);
        i--;j--;
      } else if(h[i-1][j] < h[i][j-1]){
        j--;
      } else {
        i--;
      }
    }
    
    return s;
  }
  
}