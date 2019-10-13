
public class LCS {
	
	public final static int UNKNOWN = -1;
	public final static IntSList UNKNOWNList = new IntSList(-1,null);
	
	public static int llis( int[] s ) { // s[i] > 0 per i in [0,n-1], dove n = s.length
		 	return llisRec( s, 0, 0 );
		 }


		 public static int llisRec( int[] s, int i, int t ) {
			 final int n = s.length;
			 if ( i == n ) {
				 return 0;
			 } else if ( s[i] <= t ) {
				 return llisRec( s, i+1, t );
			 } else {
				 return Math.max( 1+llisRec(s,i+1,s[i]), llisRec(s,i+1,t) );
			 }
		 }
		 
		 
		 public static int llisMem( int[] s ) { // s[i] > 0 per i in [0,n-1], dove n = s.length
			 int n = s.length;
			 int[][] acc = new int[n+1][n+1];
			 for(int i = 0; i <= n; i++) {
				 for(int j = 0; j <= n; j++) {
					 acc[i][j] = UNKNOWN;
				 }
			 }
			 	return llisRecMem( s, 0, 0, acc );
			 }


		 
		 
		 //PARTE 1
			 public static int llisRecMem( int[] s, int i, int t, int[][] acc) {
				 final int n = s.length;
				 if(acc[i][t] == UNKNOWN) {
					 if ( i == n ) {
						 acc[i][t] = 0;
					 } else if ( s[i] <= shift(s,t) ) {
						 acc[i][t] =  llisRecMem( s, i+1, t ,acc);
					 } else {
						 acc[i][t] = Math.max( 1+llisRecMem(s,i+1,i+1, acc), llisRecMem(s,i+1,t,acc) );
					 }
				 }
				 return acc[i][t];
			 }
			 
			 private static int shift(int[] s,int t) {
				 if(t == 0) {
					 t = 0;
				 }else {
					 t = s[t-1];
				 }
				 return t;
			 }
			 
			 
			 //PARTE 2
			 public static IntSList lis( int[] s ) { // s[i] > 0 per i in [0,n-1], dove n = s.length
				 int n = s.length;
				 IntSList[][] acc = new IntSList[n+1][n+1];
				 for(int i = 0; i <= n; i++) {
					 for(int j = 0; j <= n; j++) {
						 acc[i][j] = UNKNOWNList;
					 }
				 }
				 	return lisRec( s, 0, 0, acc );
				 }


				 public static IntSList lisRec( int[] s, int i, int t, IntSList[][] acc) {
					 final int n = s.length;
					 if(acc[i][t] == UNKNOWNList) {
						 if ( i == n ) {
							 acc[i][t] = new IntSList();
						 } else if ( s[i] <= shift(s,t) ) {
							 acc[i][t] =  lisRec( s, i+1, t ,acc);
						 } else  if(lisRec(s,i+1,i+1, acc).cons(s[i]).length() > lisRec(s,i+1,t,acc).length()){
							 acc[i][t] = lisRec(s,i+1,i+1, acc).cons(s[i]);
						 }else {
							 acc[i][t] = lisRec(s,i+1,t,acc);
						 }
					 }
					 return acc[i][t];
				 }
			 
			 
		 
		 
		 

		 
		 
}
