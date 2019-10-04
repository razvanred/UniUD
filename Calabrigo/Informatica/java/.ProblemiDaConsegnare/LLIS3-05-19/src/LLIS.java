
public class LLIS {
	
	private static final int UNKNOWN = -1;

	 public static int llisMem( int[] s ) { // s[i] > 0 per i in [0,n-1], dove n = s.length
		 int n = s.length;
		 int[][] acc = new int[n+1][n+1];
		 for(int i=0;i<n;i++) {
			 for(int j=0;j<n;j++) {
				 acc[i][j] = UNKNOWN;
			 }
		 }
		 return llisMemRec( s, 0, 0 ,acc);
		 }

	//length of the longest increasing subsequence
		 public static int llisMemRec( int[] s, int i, int t ,int[][] acc) {
		 final int n = s.length;
		 if(acc[i][t] == UNKNOWN) {
			 if ( i == n || t == n ) {
				 acc[i][t] = 0;
				 } else if ( s[i] <= shiftIndex(s, t) ) {
					 acc[i][t] =  llisMemRec( s, i+1, t ,acc );
				 } else {
					 acc[i][t] =  Math.max( 1+llisMemRec(s,i+1,i+1,acc), llisMemRec(s,i+1,t,acc) );
			 }
		 }
		 return acc[i][t];
		}
		 
		 private static int shiftIndex(int[] s, int i){ 
			    int t;
			    if(i==0){  
			      t=0;
			    } else { 
			      t=s[i-1];
			    }
			    return t;
			  }
		 
		 //BOTTOM UP
		 

		 public static int llisBM( int[] s ) {
			 int n = s.length;
			 int[][] acc = new int[n+1][n+1];
			 for(int i = 0;i < n; i++) {
				 acc[i][n] = 0;
			 }
			 for(int j = 0;j < n; j++) {
				 acc[n][j] = 0;
			 }
			 
			 for(int i=n-1;i>=0;i--){   
			      for(int t=n-1;t>=0;t--){   
			        if(s[i] <= shiftIndex(s, t)){ 
			          acc[i][t] = acc[i+1][t];            
			        } else {                        
			          acc[i][t] = Math.max(1 + acc[i+1][i+1],  
			                             acc[i+1][t]);        
			        }
			        System.out.println("i : " + i + " t : " + t + " acc :   " + acc[i][t]);
			      }
			    }
			    return acc[0][0]; 
		 }
}
