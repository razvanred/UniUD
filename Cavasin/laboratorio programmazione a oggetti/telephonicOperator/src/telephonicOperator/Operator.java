package telephonicOperator;

import java.util.ArrayList;

public class Operator{
	private ArrayList<Sim> sims;

	public Operator(){
		sims=new ArrayList<Sim>();
	}

	public void test(){
		addSim(new Sim("0123456789","012",new Owner("Alesk","Nand")));
		addSim(new Sim(new Sim("1123456789","012",new Owner("Alesk","Nand"))));
		System.out.println(toString());

	}

	public void addSim(Sim sim){
		sims.add(sim);
	}

	public void removeSim(Sim sim){
		sims.remove(sim);
	}

	@Override
	public String toString(){
		return "Operator [sims=\n"+sims+"\n]";
	}
}
