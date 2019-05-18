public class HNode implements Comparable<HNode>{
  
  private final int we;
  private final HNode ln,rn;
  private final char ch;
  
  public HNode( char c, int w){
    ch=c;
    we=w;
    ln=null;
    rn=null;
  }
  
  public HNode( HNode l, HNode r ){
    ch='\0';
    we=l.weight()+r.weight();
    ln=l;
    rn=r;
  }
  
  public boolean isLeaf(){
    return ln == null;
  }
  
  public int weight(){
    return we;
  }
  
  public char character(){
    return ch;
  }
  
  public HNode left(){
    return ln;
  }
  
  public HNode right(){
    return rn;
  }
  
  public int compareTo(HNode o){
    if (weight() < o.weight()){
      return -1;
    } else if (weight() > o.weight()){
      return +1;
    }
    return 0;
  }
  
  
  public String toString(){
    return ""+weight();
  }
  
  public String codAlbero(){
    if(isLeaf()){
      if(ch=='@' || ch=='\\'){
        return  "\\"+ch;
      }
      return ""+ch;
    }
    return "@"+ln.codAlbero()+rn.codAlbero();
  }
  
  public String[] codesTab(){
    String[] codes=new String[256];
    codesTabRec("",codes);
    return codes;
  }
  
  private void codesTabRec(String a, String[] codes){
    if(isLeaf()){
      codes[ch]=a;
    } else {
      ln.codesTabRec(a+"0",codes);
      rn.codesTabRec(a+"1",codes);
    }
  }
}