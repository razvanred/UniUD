public class Main {
  
  public static void main(String[] args) {
    Huffman.compress(args[0],"small_"+args[1]);
    Huffman.decompress("small_"+args[1],"big_"+args[0]);
  }
}
