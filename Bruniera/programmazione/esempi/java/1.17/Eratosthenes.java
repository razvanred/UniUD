import java.util.Vector;

public class Eratosthenes{
  
  public static Vector<Integer> eratothenes(int n){
    Vector<Integer> primi=new Vector<Integer>();
    
    boolean[] crivello=new boolean[n+1];
    
    for(int i=2;i<=n;i++){
      crivello[i]=true;
    }
    
    int p=2;
    while(p<=n){
      if(crivello[p]){
        primi.add(p);
        for(int m=2*p;m<=n;m+=p){
          crivello[m]=false;
        }
      }
      p+=2;
    }
    
    return primi;
  }
  
}