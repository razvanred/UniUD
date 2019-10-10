
public class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		StringSList s = new StringSList("ciao",null);
		s = s.Cons("20");
		s = s.Cons("30");
		s = s.Cons("40");
		StringSList a = Main.btrConsecutivesValues("+-", 5);
		System.out.println(a.Length());
		System.out.println(a.toString());
		
	}
	
	public static StringSList btrConsecutivesValues(String btrNum,int n) {
		StringSList s = new StringSList(btrNum,null);
		
		for(int i = 1; i < n; i++) {
			s = s.Cons(Main.btrSucc(btrNum));
			btrNum = Main.btrSucc(btrNum);
			
		}
		
		return s;
	}
	
	public static String btrSucc(String btr){
		if(btr.length() == 1) {
			if(btr.charAt(btr.length() - 1) == '+') {
				return "+-";
			}else {
				return "+";
			}
		}
		if(btr.charAt(btr.length() - 1) == '+') {
			return btrSucc(btr.substring(0, btr.length() - 1)) + "-";
		}else {
			if(btr.charAt(btr.length() - 1) == '-'){
				return btr.substring(0, btr.length() - 1) + ".";
			}else {
				return btr.substring(0, btr.length() - 1) + "+";
			}
		}
	}

}
