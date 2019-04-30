public class Knuth {
  
  public static int conta(int n){
    RoundTable t = new RoundTable(n);
    
    while(t.numCavalieri() > 1){
      t = t.esceCavaliereEPassaBrocca();
    }
    
    return t.chiHaLaBrocca();
  }
}