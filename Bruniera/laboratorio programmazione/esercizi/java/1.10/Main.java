public class Main{
  public static void main(String[] argc){
   System.out.println(btrSucc("+.-+-.")); 
   System.out.println(btrSucc("+.-+--"));
   System.out.println(btrSucc("+.-.++"));
   System.out.println(btrSucc(".")); 
   System.out.println(btrSucc("-"));
   System.out.println(btrSucc("+"));
   System.out.println(onesComplement2("101000110110101"));
   System.out.println(onesComplement2("001110110001011"));
   System.out.println(onesComplement("101000110110101"));
   System.out.println(onesComplement("001110110001011"));
  }
  
  public static String btrSucc(String btr){
    if(btr.length()==1){
      if(btr=="-"){
        return ".";
      } else if(btr=="."){
        return "+";
      }
      return "+-";
    }
    if(btr.charAt(btr.length()-1)=='-'){
      return btr.substring(0,btr.length()-1)+".";
    } else if(btr.charAt(btr.length()-1)=='.'){
      return btr.substring(0,btr.length()-1)+"+";
    }
    return btrSucc(btr.substring(0,btr.length()-1))+"-";
  }
  
  public static String onesComplement(String bin){
    String a="";
    for(int i=0;i<bin.length();i++){
      if(bin.charAt(i)=='0'){
        a+="1";
      } else {
        a+="0";
      }
    }
    return a;
  }
  
   public static String onesComplement2(String bin){
    return bin.replace('0','.')
      .replace('1','0')
      .replace('.','1');
  }
}