public class Main {
  
  public static void main(String[] args) { 
    Huffman.compress("sample_1.txt", "small_sample_1.txt");
    Huffman.compress("sample_2.txt", "small_sample_2.txt");
    Huffman.compress("sample_3.txt", "small_sample_3.txt");
    
    Huffman.decompress("small_sample_1.txt", "big_sample_1.txt");
    Huffman.decompress("small_sample_2.txt", "big_sample_2.txt");
    Huffman.decompress("small_sample_3.txt", "big_sample_3.txt");
    
    Huffman.standardCompress("sample_1.txt", "std_small_sample_1.txt");
    Huffman.standardCompress("sample_2.txt", "std_small_sample_2.txt");
    Huffman.standardCompress("sample_3.txt", "std_small_sample_3.txt");
    
    Huffman.standardDecompress("std_small_sample_1.txt", "std_big_sample_1.txt");
    Huffman.standardDecompress("std_small_sample_2.txt", "std_big_sample_2.txt");
    Huffman.standardDecompress("std_small_sample_3.txt", "std_big_sample_3.txt");
  }
  
}
