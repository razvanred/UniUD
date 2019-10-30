package telephonicOperator;

public class Promotion{
	private String name;
	private double price;

	public Promotion(String name, double price){
		this.name=name;
		this.price=price;
	}

	public Promotion(){
		name="none";
		price=0;
	}

	public String getName(){
		return name;
	}

	public double getPrice(){
		return price;
	}

	@Override
	public boolean equals(Object obj){
		if(this==obj)
			return true;
		if(obj==null)
			return false;
		if(getClass()!=obj.getClass())
			return false;
		Promotion other=(Promotion)obj;
		if(name==null){
			if(other.name!=null)
				return false;
		}
		else
			if(!name.equals(other.name))
				return false;
		return true;
	}

	@Override
	public String toString(){
		return "Promotion [name="+name+", price="+price+"]";
	}
}
