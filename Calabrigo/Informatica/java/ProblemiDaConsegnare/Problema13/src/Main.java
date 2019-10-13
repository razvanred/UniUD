import puzzleboard.*;
public class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		int n = 4;
		int[][] config = {{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,0}};
		Tavoletta t = new Tavoletta(n, config );
		//System.out.println(t.isDowelsOrdered());
		//System.out.println(t.canMoveDowel(2, 3));
		System.out.println(t.configToString());
		//ciclo di gioco
		while(true) {
			int[] posArray;
			posArray = t.findPosition(t.board.get());
			int iF = posArray[0],jF = posArray[1];
			t.moveDowel(iF, jF);
			System.out.println(t.configToString());
		}
		
	}

}


/*
 * Il protocollo della classe PuzzleBoard prevede un costruttore
 * per costruire una "tavoletta" n x n:
 *
 *   PuzzleBoard board = new PuzzleBoard( n );
 *
 * e i seguenti metodi:
 *
 *   board.setNumber( i,j, k ); 
 *
 * per definire o modificare il valore della pedina di coordinate
 * <i,j>, dove i, j appartengono all'intervallo di indici [1,n];
 *
 *   board.clear( i,j );
 *
 * per cancellare la pedina di coordinate <i,j>, dove i, j in [1,n]
 * (di conseguenza, in posizione <i,j> si crea una lacuna);
 *
 *   board.display();
 *
 * per aggiornare la visualizzazione della tavoletta;
 *
 *   int k = board.get();
 *
 * per acquisire il valore numerico della pedina selezionata
 * dall'utente attraverso il mouse (il programma che invoca
 * il metodo attiva il processo di selezione e viene sospeso
 * fino a selezione avvenuta attraverso l'operazione "click").
 *
*/
