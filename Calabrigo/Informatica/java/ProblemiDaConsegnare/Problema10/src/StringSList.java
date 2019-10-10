
public class StringSList {
	boolean empty;
	StringSList rest;
	String first;
	
	public StringSList() {
		empty = true;
		rest = null;
	}
	
	public StringSList(String first,StringSList rest) {
		this.first = first;
		this.rest = rest;
		this.empty = false;
	}
	
	public String Car() {
		return first;
	}
	
	public StringSList Cdr() {
		return rest;
	}
	
	public StringSList Cons(String str) {
		return new StringSList(str ,this);
	}
	
	public int Length() {
		StringSList q = this;
		int counter = 0;
		while(q.rest != null) {
			q = q.Cdr();
			counter++;
		}
		return counter + 1;
	}
	
	public String StringRef(int index) {
		StringSList q = this;
		int counter = 0;
		while(counter != index) {
			q = q.Cdr();
			counter++;
		}
		return q.first;
	}
	
	public String toString() {
		String v = "";
		v = v + "(" + this.Car();
		StringSList q = this.rest;
		while(q != null) {
			v = v + ", " + q.Car();
			q = q.Cdr();
		}
		return v + ")";
	}
	
	public StringSList Append(StringSList list) {
		StringSList q = this;
		while(list != null) {
			q = q.Cons(list.Car());
			list = list.Cdr();
		}
		return q;
	}
	
	public StringSList Reverse() {
		StringSList q = this;
		StringSList r = new StringSList();
		while(q != null) {
			r = r.Cons(q.Car());
			q = q.Cdr();
		}
		return r;
	}

}
