public class LLCS {
	
	//parte 1
	
	private static final int UNKNOWN = -1;
	
	public static int llcs3(String s1, String s2, String s3) {
		int[][][] acc = new int[s1.length() + 1][s2.length() + 1][s3.length() + 1];
		for(int i = 0;i <= s1.length();i++) {
			for(int j = 0;j <= s2.length();j++) {
				for(int k = 0;k <= s3.length();k++) {
					acc[i][j][k] = UNKNOWN;
				}
			}
		}
		return llcs3Mem(s1, s2, s3,s1.length(),s2.length(),s3.length(), acc);
	}
	
	public static int llcs3Mem(String s1, String s2, String s3,int length1,int length2,int length3, int[][][] acc) {
		if(acc[length1-s1.length()][length2-s2.length()][length3-s3.length()] == UNKNOWN) {
			if(s1.length() == 0 || s2.length() == 0 || s3.length() == 0) {
				acc[length1-s1.length()][length2-s2.length()][length3-s3.length()] =  0;
			}else if((s1.charAt(0) == s2.charAt(0)) && (s2.charAt(0) == s3.charAt(0))) {
				acc[length1-s1.length()][length2-s2.length()][length3-s3.length()] =
						 1 + llcs3Mem(s1.substring(1),s2.substring(1),s3.substring(1),length1,length2,length3,acc);
			}else {
				acc[length1-s1.length()][length2-s2.length()][length3-s3.length()] = 
						Math.max(Math.max(llcs3Mem(s1.substring(1),s2,s3,length1,length2,length3,acc),
						llcs3Mem(s1,s2.substring(1),s3,length1,length2,length3,acc)),
						llcs3Mem(s1,s2,s3.substring(1),length1,length2,length3,acc));
			}
		}
		return acc[length1-s1.length()][length2-s2.length()][length3-s3.length()];
	}
	
	//parte 2
	
	public static boolean isMatrixSymmeter(int[][] matrix) {
		boolean symmeter = true;
		int n = matrix.length;
		
		for(int i = 0;i < n - 1 && symmeter;i++) {
			for(int j = i + 1;j < n; j++) {
				if(matrix[i][j] != matrix[j][i]) {
					symmeter = false;
				}
			}
		}
		
		return symmeter;
	}

}
