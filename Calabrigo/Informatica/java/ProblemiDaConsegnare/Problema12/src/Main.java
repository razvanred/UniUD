import queens.*;
public class Main {

	public static void main(String[] args) {
		//part 1
		SList<Integer> s = new SList<Integer>();
		s = s.cons(3);
		s = s.cons(4);
		s = s.cons(5);
		s = s.cons(6);
		System.out.println(s.toString());
		Queens q = new Queens();
		//
		int n = 7;//grandezza della board
	    System.out.println( q.numberOfSolutions(n) );

	    //part 2
	    ChessboardView gui = new ChessboardView(n);
	    q.queensConfiguration(gui, n);
	}

}
