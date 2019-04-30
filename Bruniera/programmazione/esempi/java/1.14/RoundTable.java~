public class RoundTable{
  
  private final IntSList tav;
  private final boolean serv;
  private final IntSList usc;
  
  public RoundTable(int n){
    tav=range(1,n);
    usc=IntSList.NULL_INTLIST;
    serv=false;
  }
  
  private RoundTable(IntSList tav, IntSList usc, boolean serv){
    this.tav=tav;
    this.usc=usc;
    this.serv=serv;
  }
  
  private static IntSList range(int inf, int sup){
    IntSList il= IntSList.NULL_INTLIST;
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
    return new RoundTable(tav.cdr().append(IntSList.NULL_INTLIST.cons(tav.car())),
                          usc,
                          false);
  }
  
  public RoundTable esceCavaliere(){
    return new RoundTable(tav.cdr().cdr().cons(tav.car()),
                             usc.cons(tav.cdr().car()),
                             true);
  }
  
  public RoundTable esceCavaliereEPassaBrocca(){
    return new RoundTable(tav.cdr().cdr().append(IntSList.NULL_INTLIST.cons(tav.car())),
                          usc.cons(tav.cdr().car()),
                          false);
    
  }
  
  public IntSList cavalieriUsciti(){ 
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