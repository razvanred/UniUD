public class Main {
  
  public static void main(String[] argc){
    System.out.println(btrSucc("+.-+-.")); 
    System.out.println(btrSucc("+.-+--"));
    System.out.println(btrSucc("+.-.++"));
    System.out.println(onesComplement("101000110110101"));
    System.out.println(onesComplement("001110110001011"));
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
  
  public static String onesComplement(String bin){
    char binArray[]=bin.toCharArray();
    for(int i=bin.length();i>0;i--){
      if(binArray[i-1]=='0')
        binArray[i-1]='1';
      else
        binArray[i-1]='0';
    }
    return String.valueOf(binArray);
  }
}
