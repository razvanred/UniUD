public class Memoization{
  
  public static int llcs3(String t, String u, String v){
    int lt=t.length(),
      lu=u.length(),
      lv=v.length();
    int[][][] h=new int[lt+1][lu+1][lv+1];
    
    for(int x=0;x<=lt;x++){
      for(int y=0;y<=lu;y++){
        h[x][y][0]=0;
      }
    }
    
    for(int x=0;x<=lt;x++){
      for(int z=0;z<=lv;z++){
        h[x][0][z]=0;
      }
    }
    
    for(int y=0;y<=lu;y++){
      for(int z=0;z<=lv;z++){
        h[0][y][z]=0;
      }
    }

    for(int x=1;x<=lt;x++){
      for(int y=1;y<=lu;y++){
        for(int z=1;z<=lv;z++){
          if(t.charAt(lt-x) == u.charAt(lu-y) &&  u.charAt(lu-y) == v.charAt(lv-z)){
            h[x][y][z]=h[x-1][y-1][z-1]+1;
          } else {
            h[x][y][z]=Math.max(h[x-1][y][z],Math.max(h[x][y-1][z],h[x][y][z-1]));
          }
        }
      }
    }
    
    return h[lt][lu][lv];
  }
  
}