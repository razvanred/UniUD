public class RoundTable{
  
  private SList<Integer> tav;
  private SList<Integer> usc;
  private int num;
  private static final SList<Integer> NULL_LIST = new SList<Integer>();
  
  public RoundTable(int n){
    tav=range(1,n);
    usc=NULL_LIST;
    serv=false;
    num=n;
  }
  
  private static SList<Integer> range(int inf, int sup){
    SList<Integer> il= NULL_LIST;
    while(inf<=sup){
      il=il.cons(sup);
      sup--;
    }
    return il;
  }
  
  public int numCavalieri(){
    return tav.length();
  }
  
  public int chiHaLaBrocca(){
    return tav.car();
  }
  
  public boolean servitoCavaliere(){
    return serv;
  }
  
  public int cavASinistra(){
    return tav.cdr().car();
  }
  
  public RoundTable passaBrocca(){
    return new RoundTable(tav.cdr().append(NULL_LIST.cons(tav.car())),
                          usc,
                          false);
  }
  
  public RoundTable esceCavaliere(){
    return new RoundTable(tav.cdr().cdr().cons(tav.car()),
                             usc.cons(tav.cdr().car()),
                             true);
  }
  
  public RoundTable esceCavaliereEPassaBrocca(){
    SList<Integer> u = tav.cdr();
    SList<Integer> v = usc.cons(tav.car());
    if(){
      
    } else if() {
      tav = v.reverse();
      tav = r.cdr();
    } else {
      tav=tav.cdr();
      usc=v;
    }
    num--;
  }
  
  public SList<Integer> cavalieriUsciti(){ 
    return usc;
  }
  
  public static int conta(int n){
    RoundTable t=new RoundTable(n);
    while(t.numCavalieri()>1){
      t=t.esceCavaliereEPassaBrocca();
    }
    return t.chiHaLaBrocca();
  }
}