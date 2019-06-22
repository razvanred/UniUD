public class Main {
  
  public static void main(String[] argc){
    System.out.println(btrNSucc("+.-+-.",1)); 
    System.out.println(btrNSucc("+.-+--",3));
    System.out.println(btrNSucc("+.-.++",5));
  }
  
  
  public static StringSList btrNSucc(String btr, int n){
    StringSList list=new StringSList();
    for(;n>0;n--){
      list=list.append(new StringSList(btr,StringSList.NULL_STRINGLIST));
      btr=btrSucc(btr);
    }
    return list;
  }
  
  public static String btrSucc(String btr){
    int n=btr.length();
    char lsb= btr.charAt(n-1);
    if(n==1){
      if(lsb=='+')
        return "+-";
      else
        return "+";
    }
    String pre= btr.substring(0,n-1);
    if(lsb=='+')
      return btrSucc(pre)+"-";
    else{
      if(lsb=='-')
        return pre+".";
      else
        return pre+"+";
    }    
  }
  
}
