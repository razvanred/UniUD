import puzzleboard.*;

public class Main {
  
  public static void main(String[] args) {
    int n=Integer.parseInt(args[0]);
    
    Puzzle puzzle=new Puzzle(n, new PuzzleBoard(n));
    
    puzzle.play();
  }
  
}
