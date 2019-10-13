public class Symmetry{
  
  public static boolean symmetry(int[][] mat){
    boolean sym=true;
    int it=mat.length-1;
    
    for(int i=0;i<it && sym;i++){
      for(int j=1+i;j<mat.length && sym;j++){
        if(mat[i][j]!=mat[j][i]){
          sym=false;
        }
      }
    }
    
    return sym;
  }
}