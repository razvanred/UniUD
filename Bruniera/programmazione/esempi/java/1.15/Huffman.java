import huffman_toolkit.*;
import java.util.*;
 
public class Huffman{
  
  public static int[] tabOccorrenze(String doc){
    int[] tab=new int[256];
    InputTextFile itf=new InputTextFile(doc);
    while(itf.textAvailable()){
      char c=itf.readChar();
      tab[c]++;
    }
    itf.close();
    return tab;
  }
  
  public static void creaCoda( int[] tab ){
    PriorityQueue<HNode> q =new PriorityQueue<HNode>();
    for(int i=0;i<256;i++){
      if( tab[i]>0 ){
        HNode n =new HNode( (char) i, tab[i] );
        q.add(n);
      }
    }
    
    while(q.size()>0){
      HNode n=q.poll();
      System.out.println(""+n);
    }
  }
  
  public static HNode hAlbero( int[] tab ){
    PriorityQueue<HNode> q =new PriorityQueue<HNode>();
    for(int i=0;i<256;i++){
      if( tab[i]>0 ){
        HNode n =new HNode( (char) i, tab[i] );
        q.add(n);
      }
    }
    
    while(q.size()>1){
      HNode l=q.poll();
      HNode r=q.poll();
      q.add(new HNode(l,r));
    }
    return q.poll();
  }
  
  public static void comprimi(String doc, String com){
    int[] tab=tabOccorrenze(doc);
    HNode t=hAlbero(tab);
    int count=t.weight();
    
    //String[] codes=t.codesTab();
    String[] codes=tabCodici(t);
    
    InputTextFile itf=new InputTextFile(doc);
    OutputTextFile otf=new OutputTextFile(com);
    
    otf.writeTextLine(""+t.weight());
    otf.writeTextLine(t.codAlbero());
    
    for(int i=0;i<count;i++){
      char c=itf.readChar();
      
      otf.writeCode(codes[c]);
    }
    itf.close();
    otf.close();
  }
  
  public static void decomprimi(String com, String doc){
    InputTextFile in=new InputTextFile(com);
    OutputTextFile out=new OutputTextFile(doc);
    
    String num=in.readTextLine();
    int count = Integer.parseInt(num);
    
    HNode t=new HNode(in);
    
    in.readTextLine(); //elimina un lf
    for(int i=0;i<count;i++){
      out.writeChar(t.readChar(in));
    }
    in.close();
    out.close();
  }
  
  /////////////////////////////////////////////
  
  public static String[] tabCodici(HNode n){
    
    String[] tab=new String [256];
    
    Stack<Pair> s=new Stack<Pair>();
    
    s.push(new Pair(n,""));
    
    while(!s.empty()){
      
      Pair cp=s.pop();
      n=cp.nodo;
      
      if(n.isLeaf()){
        char c=n.character();
        tab[c]=cp.pre;
      } else {
        s.push(new Pair(n.right(),cp.pre+"1"));
        s.push(new Pair(n.left(),cp.pre+"0"));
      }
    }
    return tab;
  }
      
}