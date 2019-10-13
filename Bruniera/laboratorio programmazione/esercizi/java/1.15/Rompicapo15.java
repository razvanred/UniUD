public class Rompicapo15{
  public static void rompicapo(int n){
    Tavoletta table=new Tavoletta(n);
    System.out.println(table.toString());
    do{
      table.display();
      int value=table.get();
      if(table.isMovable(value)){
        System.out.println("yes "+value);
        table.move(value);
      } else {
        System.out.println("no "+value);
      }
    //}while(!table.isOrdered());
    }while(true);
  }
}