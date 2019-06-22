import puzzleboard.*;
import java.util.Vector;
import java.util.concurrent.ThreadLocalRandom;

public class Puzzle {
  private PuzzleBoard gui;
  private int n,board[][];
  
  public Puzzle(int n, PuzzleBoard gui){
    this.n=n;
    this.gui=gui;
    board=new int[n][n];
    
    Vector<Integer> numbers=new Vector<Integer>(n*n);
    for(int i=0;i<n*n;i++){
      numbers.add(i);
    }
    
    System.out.println(numbers.toString());
    
    for(int i,y=0;y<n;y++){
      for(int x=0;x<n;x++,i++){
        i=ThreadLocalRandom.current().nextInt(0, numbers.size());
        board[x][y]=numbers.elementAt(i);
        gui.setNumber(y+1,x+1,numbers.elementAt(i));
        numbers.removeElementAt(i);
      }
    }
  }
  
  public boolean isOrdered(){
    for(int i=1,y=0;y<n;y++){
      for(int x=0;x<n;x++){
        if(board[x][y]!=0){
          if(board[x][y]!=i){
            return false;
          }
          i++;
        }
      }
    }
    return true;
  }
  
  public boolean canBeMoved(int x,int y){
    if(x>0&&board[x-1][y]==0||x<n-1&&board[x+1][y]==0||y>0&&board[x][y-1]==0||y<n-1&&board[x][y+1]==0){
      return true;
    }
    return false;
  }
  
  public void play(){
    int x,y,v;
        
    while(!isOrdered()){
      gui.display();
      v=gui.get();
      for(y=0;y<n;y++){
        for(x=0;x<n;x++){
          if(board[x][y]==v&&canBeMoved(x,y)){
            move(x,y);
            x=n;
            y=n;
          }
        }
      }
    }
    System.out.println("completed");
  }
  
  public String toString(){
    String s="";
    for(int y=0;y<n;y++){
      for(int x=0;x<n;x++){
        s=s+board[x][y]+"\t";
      }
      s+="\n";
    }
    return s;
  }
  
  public void move(int x,int y){
    int t=board[x][y];
    board[x][y]=0;
    gui.clear(y+1,x+1);
    if(x>0&&board[x-1][y]==0){
      board[x-1][y]=t;
      gui.setNumber(y+1,x,t);
    }else if(x<n-1&&board[x+1][y]==0){
      board[x+1][y]=t;
      gui.setNumber(y+1,x+2,t);
    }else if(y>0&&board[x][y-1]==0){
      board[x][y-1]=t;
      gui.setNumber(y,x+1,t);
    }else if(y<n-1&&board[x][y+1]==0){
      board[x][y+1]=t;
      gui.setNumber(y+2,x+1,t);
    }else{
      board[x][y]=t;
      gui.setNumber(y+1,x+1,t);
    }
  }
}
