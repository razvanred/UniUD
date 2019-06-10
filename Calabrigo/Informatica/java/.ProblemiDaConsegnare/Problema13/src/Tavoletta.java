import java.lang.reflect.Array;

import puzzleboard.PuzzleBoard;

public class Tavoletta {
	private static int n; //k è il numero del tassello <i,j> (che va da 1 a n^2 - 1)
	private static int[][] tavoletta;
	public static PuzzleBoard board;
	

	public Tavoletta(int n,int[][] configuration) {
		//il tassello vuoto, avrà un valore pari a zero
		this.n = n;
		this.board = new PuzzleBoard( this.n );
		this.tavoletta = configuration;
		for(int i = 0;i < n;i++) {
			for(int j=0;j<n;j++) {
				board.setNumber(i+1, j+1, tavoletta[i][j]);
			}
		}
	}
	
	//controllo se i tasselli sono in ordine
	public static boolean isDowelsOrdered() {
		int isOrdered = 0;
		for(int i=0;i<n;i++) {
			for(int j=0;j<n;j++) {
				if(tavoletta[i][j] < nextDowel(i, j)) {
					isOrdered++;
				}
			}
		}
		System.out.println(isOrdered);
		if(isOrdered == (n*n)-2) {
			return true;
		}
		return false;
	}
	
	//ritorna la prossima tavoletta, ritorna -1 se non c'è una prossima tavoletta
	public static int nextDowel(int i,int j) {
		int n = Array.getLength(tavoletta);
		if(j >= n-1 && i < n-1) {
			return tavoletta[i+1][0];
		}else if(j >= n-1 && i >= n-1) {
			return -1;
		}else {
			return tavoletta[i][j+1];
		}
	}
	
	//verifico se posso spostare il tassello <i,j>, (il tassello vuoto è identificato con k=0)
	public static boolean canMoveDowel(int i,int j) {
		
		boolean canMove = false;
		
		if(j < n-1) {
			if(tavoletta[i][j+1] == 0) {
				canMove = true;
			}
		}
		if( j > 0 ) {
			if(tavoletta[i][j-1] == 0) {
				canMove = true;
			}
		}
		if(i < n-1) {
			if(tavoletta[i+1][j] == 0) {
				canMove = true;
			}
		}
		if(i > 0) {
			if(tavoletta[i-1][j] == 0) {
				canMove = true;
			}
		}
		return canMove;
	}
	
	public static String configToString() {
		String confString = "";
		
		for(int i = 0;i < n;i++) {
			for(int j=0;j<n;j++) {
				confString += "Tassello <i,j> : <" + i + "," + j +"> = " + tavoletta[i][j] + "\n";
			}
		}
		return confString;
	}
	
	public static void moveDowel(int iI,int jI) {
		int temp;
		//find the landing position for the dowel
		int[] posArray;
		posArray = findPosition(0);
		int iF = posArray[0],jF = posArray[1];
		//System.out.println(posArray[0] +"  " +posArray[1]);
		//verify if the dowel can move to that position, and moves it
		if(canMoveDowel(iI, jI) && iF >= iI - 1 && iF <= iI + 1 && jF >= jI - 1 && jF <= jI + 1) {
			if(!((iF == iI + 1 && jF == jI + 1) || (iF == iI - 1 && jF == jI + 1) || (iF == iI + 1 && jF == jI - 1) || (iF == iI - 1 && jF == jI - 1))) {
				temp = tavoletta[iI][jI];
				tavoletta[iI][jI] = tavoletta[iF][jF];
				tavoletta[iF][jF] = temp;
				board.clear(iI+1, jI+1);//tavoletta[iI][jI] = 0;
				board.setNumber(iF+1, jF+1, tavoletta[iF][jF]);
				board.display();
			}
		}
	}
	
	public static int[] findPosition(int k) {
		int iF = 0,jF = 0;
		for(int i = 0;i < n;i++) {
			for(int j = 0;j < n;j++) {
				if(tavoletta[i][j] == k) {
					//System.out.println(tavoletta[i][j]);
					iF = i;
					jF = j;
				}
			}
		}
		int[] posArray = {iF,jF};
		//System.out.println(posArray[0] +"  " +posArray[1]);
		return posArray;
	}
	
	

}
