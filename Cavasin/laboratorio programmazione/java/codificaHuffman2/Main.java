public class Main {
  
  public static void main(String[] args) {
    Huffman.compress(args[0],"t");
    Huffman.decompress("t","huffman_"+args[0]);
    MyHuffman.compress(args[0],"t");
    MyHuffman.decompress("t","myHuffman_"+args[0]);
    
    String t[]=Huffman.huffmanCodesTable(Huffman.huffmanTree(Huffman.charHistogram(args[0])));
    String p[]=MyHuffman.huffmanCodesTableIterative(Huffman.huffmanTree(MyHuffman.charHistogram(args[0])));
    
    for(int i=0;i<t.length;i++){
      if(t[i]!=null&&p[i]!=null&&!t[i].equals(p[i])){
        System.out.println("false");
        return;
      }
    }
    System.out.println("true");
    
  }
}
