// 
public class Main {

	public static void main(String[] args) {
		Main m = new Main();
		System.out.println(m.btrSucc("--"));
		System.out.println(m.onesComplement("101010"));

	}
	
	public String btrSucc(String btr){
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
	
	public static String onesComplement(String bin) {
		if(bin.isEmpty()) {
			return "";
		}else {
			return ExchangeBit(bin.charAt(0)) + onesComplement(bin.substring(1, bin.length()));
		}
	}
	
	public static String ExchangeBit(char bit) {
		if(bit == '1') {
			return "" + '0';
		}else {
			return "" + '1';
		}
	}

}
