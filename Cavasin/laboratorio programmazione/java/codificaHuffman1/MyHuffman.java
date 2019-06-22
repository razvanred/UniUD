import huffman_toolkit.*;
import java.util.*;

public class MyHuffman {
  private static final int LENGTH=256;
  
  private static char read8bit(InputTextFile in){
    char t=0;
    for(int i=0;i<7/*&&in.bitsAvailable()*/;i++){
      t<<=1;
      t+=in.readBit();
    }
    System.out.print(t);
    return t;
  }
  
  public static int[] charHistogram(String src){
    InputTextFile in=new InputTextFile(src);
    int freq[]=new int[LENGTH];
    
    while (in.bitsAvailable()){
      char t=read8bit(in);
      freq[t]++;
    }
    in.close();
    return freq;
  }
  
  public static Node huffmanTree(int freq[]){
    PriorityQueue<Node> nodes = new PriorityQueue<Node>();
    Node l,r;
    
    for(int i=0;i<LENGTH;i++){
      if(freq[i]>0){
        nodes.add(new Node((char)i,freq[i]));
      }
    }
    while(nodes.size()>1){
      l=nodes.poll();
      r=nodes.poll();
      nodes.add(new Node(l,r));
    }
    return nodes.poll();
  }
  
  public static String[] huffmanCodesTable(Node root){
    String codes[]=new String[LENGTH];
    
    fillTable(codes,root,"");
    return codes;
  }
  
  private static void fillTable(String codes[],Node node,String code){
    if(node.isLeaf()){
      codes[node.character()]=code; 
    }else{
      fillTable(codes,node.left(),code+'0');
      fillTable(codes,node.right(),code+'1');
    }
  }
  
  public static String flattenTree(Node node){
    if(node.isLeaf()){
      char t=node.character();
      if((t=='\\')||(t=='@')){
        return ("\\"+t);
      }else{
        return (""+t);
      }
    }else{
      return "@"+flattenTree(node.left())+flattenTree(node.right());
    }
  }

  public static void compress(String src,String dst){
    Node root=huffmanTree(charHistogram(src));
    String codes[]=huffmanCodesTable(root);
    InputTextFile in=new InputTextFile(src);
    OutputTextFile out=new OutputTextFile(dst);
    
    int length=root.weight();
    out.writeTextLine(""+length);
    out.writeTextLine(flattenTree(root));
    for(;length>0;length--){
      out.writeCode(codes[read8bit(in)]);
    }
    in.close();
    out.close();
  }
  
  public static Node restoreTree(InputTextFile in){
    int t=read8bit(in);
    if(t=='@'){
      Node left=restoreTree(in);
      Node right=restoreTree(in);
      return new Node(left,right);
    }else{
        if(t=='\\'){
          t=read8bit(in);
        }
        return new Node((char)t,0);
    }
  }
  
  public static void decompress(String src,String dst){
    InputTextFile in=new InputTextFile(src);
    int length=Integer.parseInt(in.readTextLine());
    Node root=restoreTree(in);
    
    in.readTextLine();
    
    System.out.println(flattenTree(root));
    OutputTextFile out=new OutputTextFile(dst);
    
    for(;length>0;length--){
      out.writeChar(decodeNextChar(root,in));
    }
    in.close();
    out.close();
  }
  
  private static char decodeNextChar(Node node,InputTextFile in){
    while(!node.isLeaf()){
      if(in.readBit()==0){
        node=node.left();
      }else{
        node=node.right();
      }
    }
    return node.character();
  }

  
}
