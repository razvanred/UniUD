public class Main {
  
  public static void main(String[] args) { 
    System.out.println(llisTD( new int[] {5, 4, 3, 2, 1} )); 
    System.out.println(lis( new int[] {5, 4, 3, 2, 1} ));
    
    System.out.println(llisTD( new int[] {47, 38, 39, 25, 44} ));
    System.out.println(lis( new int[] {47, 38, 39, 25, 44} ));
    
    System.out.println(llisBU( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} )); 
    System.out.println(lis( new int[] {27, 90, 7, 29, 49, 8, 53, 1, 28, 6} ));
    
    System.out.println(llisTD( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} )); 
    System.out.println(lis( new int[] {9, 46, 54, 71, 60, 47, 0, 32, 25, 61} ));
    
    System.out.println(llisTD( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} )); 
    System.out.println(lis( new int[] {54, 52, 42, 33, 14, 40, 37, 61, 53, 1} ));
    
    System.out.println(llisTD( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71}));
    System.out.println(lis( new int[] {1,76,4,453,2,34,56,8,7,43,2,34,67,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,432,845,65,76,7,45,267,4653,45,437,874,47,74,865,5,55,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,7,76,4576,76,4,7865,32,743,67,76,4,453,2,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,34,56,8,7,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,453,2,34,56,8,7,43,2,34,67,43,2,34,67,65,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,76,4,874,47,74,865,5,55,7,76,4576,76,4,7865,32,743,67,71}));
  }
  
  public static IntSList lis( int[] s ){
    int cache[][]=new int[s.length+1][s.length+1];
    for(int i=0;i<cache.length;i++){
      for(int t=0;t<cache[0].length;t++){
        cache[i][t]=-1;
      }
    }
    llisRecTD(s, 0, 0, cache);
//    printMatrix(cache);
    IntSList list=new IntSList();
    
    int i=0,t=0;
    while(cache[i][t]!=0){
      if(s[i]<=shift(s,t)){
        i++;
      }else{
        if(1+cache[i+1][i+1]>cache[i+1][t]){
          list=list.append(new IntSList().cons(s[i]));
          i++;
          t=i;
        }else{
          i++;
        }
      }
    }
    return list;
  }
  
  public static int llis( int[] s ) { // s[i] > 0 per i in [0,n-1], dove n = s.length
    return llisRec( s, 0, 0 );
  }  
  
  public static int llisRec( int[] s, int i, int t ) {
    final int n = s.length;
    if ( i == n ) {
      return 0;
    } else if ( s[i] <= t ) {
      return llisRec( s, i+1, t );
    } else {
      return Math.max( 1+llisRec(s,i+1,s[i]), llisRec(s,i+1,t) );
    }
  }
  
  public static int llisTD( int[] s ){
    int cache[][]=new int[s.length+1][s.length+1];
    for(int i=0;i<cache.length;i++){
      for(int t=0;t<cache[0].length;t++){
        cache[i][t]=-1;
      }
    }
    return llisRecTD(s, 0, 0, cache);
  }
  
  public static int llisRecTD( int[] s, int i, int t , int[][] cache){
    if(cache[i][t]!=-1){
      return cache[i][t];
    }else if(i==s.length){
      cache[i][t]=0;
    }else if(s[i]<=shift(s,t)){
      cache[i][t]=llisRecTD(s,i+1,t,cache);
    }else{
      cache[i][t]=Math.max(1+llisRecTD(s,i+1,i+1,cache),llisRecTD(s,i+1,t,cache));
    }
    return cache[i][t];
  }
  
  public static int llisBU( int[] s ){
    int cache[][]=new int[s.length+1][s.length+1];
    for(int i=0;i<cache.length-1;i++){
      for(int t=0;t<cache[0].length;t++){
        cache[i][t]=-1;
      }
    }
    for(int t=0;t<cache[0].length;t++){
      cache[cache.length-1][t]=0;
    }
    for(int i=cache.length-2;i>=0;i--){
      for(int t=cache[0].length-1;t>=0;t--){
        if(s[i]<=shift(s,t)){
          cache[i][t]=cache[i+1][t];
        }else{
          if(1+cache[i+1][i+1]>cache[i+1][t]){
            cache[i][t]=1+cache[i+1][i+1];
          }else{
            cache[i][t]=cache[i+1][t];
          }
        }
      }
    }
    return cache[0][0];
  }
  
  public static void printMatrix(int[][] matrix) {
    for(int[] row : matrix) {
      for (int t : row) {
        System.out.print(t);
        System.out.print("\t");
      }
      System.out.println();
    }
  }
  
  private static int shift(int s[],int i){
    if(i==0)
      return 0;
    else
      return s[i-1];
  }
  
}
