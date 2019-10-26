package telephonicOperator;

import java.util.ArrayList;

public class Operator{
	private ArrayList<Sim> sims;

	public Operator(){
		sims=new ArrayList<Sim>();
	}

	public void addSim(Sim sim){
		sims.add(sim);
	}

	public void removeSim(Sim sim){
		sims.remove(sim);
	}

}
