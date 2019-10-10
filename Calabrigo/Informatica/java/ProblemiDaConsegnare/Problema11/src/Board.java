
/*
	 * Board b = new Board(n) creazione di una scacchiera vuota
	b.size() : int dimensione della scacchiera
	b.queensOn () : int numero di regine collocate sulla scacchiera
	b.underAattack(i,j) : boolean la posizione di coordinate <i, j> è minacciata?
	b.addQueen(i,j) : Board nuova scacchiera con una regina in posizione <i, j>
	che si aggiunge alla configurazione di b
	b.arrangement() : String codifica testuale della configurazione
	 */
	
	/*
	la dimensione della scacchiera (int);
	• il numero di regine collocate nella scacchiera (int);
	• 4 liste di indici (IntSList), per rappresentare rispettivamente le codifiche numeriche delle righe, delle colonne,
	delle diagonali ascendenti verso destra e delle diagonali ascendenti verso sinistra che sono minacciate da una
	regina collocata sulla scacchiera;
	• la codifica testuale della configurazione secondo le convenzioni in uso da parte degli scacchisti (String).
	 */
//aDiag = 0 -1 -2 -3 -4
//dDiag = 2 3 4 5 6

public class Board {
	
	private final int dim;
	private final int nQueens;
	private final IntSList rows,columns,aDiag,dDiag;
	private final String textConf;
	
	public Board(int dim){
		this.dim = dim;
		nQueens = 0;
		rows = new IntSList();
		columns = new IntSList();
		aDiag  = new IntSList();
		dDiag  = new IntSList();
		textConf = "";
	}
	
	public Board(int dim,int nQueens,IntSList rows,IntSList columns,IntSList aDiag,IntSList dDiag,String textConf) {
		this.dim = dim;
		this.nQueens = nQueens;
		this.rows = rows;
		this.columns = columns;
		this.aDiag = aDiag;
		this.dDiag = dDiag;
		this.textConf = textConf;
	}
	
	public int size() {
		return this.dim;
	}
	
	public int queensOn() {
		return this.nQueens;
	}
	
	public boolean underAttack(int i,int j){
		
		//controllo se la riga dove mi trovo sia minacciata
		IntSList r = rows;
		for(int index = 0;index < rows.length();index++) {
			if(r.car() == i) {
				return true;
			}
			r = r.cdr();
		}
		//controllo se la colonna in cui mi trovo sia minacciata
		IntSList c = columns;
		for(int index = 0;index < rows.length();index++) {
			if(c.car() == j) {
				return true;
			}
			c = c.cdr();
		}
		//controllo se la diagonale ascendente in cui mi trovo sia minacciata
		IntSList aD = aDiag;
		for(int index = 0;index < aDiag.length();index++) {
			if(aD.car() == (i - j)) {
				return true;
			}
			aD = aD.cdr();
		}
		//controllo se la diagonale discendente in cui mi trovo sia minacciata
		IntSList dD = dDiag;
		for(int index = 0;index < aDiag.length();index++) {
			if(dD.car() == (i + j)) {
				return true;
			}
			dD = dD.cdr();
		}
		return false;
	}
	
	public Board addQueen(int i,int j) {
		//System.out.println("row: " + this.rows.toString());
		//System.out.println("aDiag: " + this.aDiag.toString());
		//System.out.println("dDiag: " + this.dDiag.toString());
		return new Board(this.dim,this.nQueens + 1,rows.cons(i),columns.cons(j),aDiag.cons(i - j),dDiag.cons(i + j),this.textConf + " , " + "Queen on " + i + j);
	}
	
	public String arrangement() {
		return this.textConf;
	}

}
