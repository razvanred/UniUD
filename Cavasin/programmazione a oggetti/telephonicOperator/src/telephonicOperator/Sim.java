package telephonicOperator;

import java.time.Duration;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;

public class Sim{
	private String number, puk;
	private LocalDate lastRecharge;
	private double credit;
	private Promotion promotion;
	private Owner owner;
	private ArrayList<Call> calls;
	private boolean ported;

	public Sim(String number, String puk, Owner owner){
		this.number=number;
		this.puk=puk;
		this.owner=owner;
		credit=0;
		promotion=new Promotion();
		lastRecharge=LocalDate.now();
		calls=new ArrayList<Call>();
		ported=false;
	}

	public Sim(Sim sim){
		number=sim.getNumber();
		puk=sim.getPuk();
		lastRecharge=sim.getLastRecharge();
		credit=sim.getCredit();
		owner=sim.getOwner();
		promotion=new Promotion();
		calls=new ArrayList<Call>();
		ported=true;
	}

	public void activatePromotion(Promotion promotion){
		this.promotion=promotion;
		credit-=promotion.getPrice();
	}

	public void deactivatePromotion(){
		promotion=new Promotion();
	}

	public void addCall(Call call){
		calls.add(call);
	}

	public boolean isActive(){
		if(Duration.between(lastRecharge,LocalDate.now()).compareTo(Duration.of(1,ChronoUnit.YEARS))<0){
			return true;
		}
		else{
			return false;
		}
	}

	public Duration getCallTime(){
		Duration d=Duration.ZERO;

		for(Call t:calls){
			d.plus(t.duration());
		}
		return d;
	}

	public ArrayList<Call> getCalls(){
		return calls;
	}

	public ArrayList<Call> getCallsTo(Sim sim){
		ArrayList<Call> t=new ArrayList<Call>();

		for(Call c:calls){
			if(sim.equals(c.receiver())){
				t.add(c);
			}
		}
		return t;
	}

	public String getNumber(){
		return number;
	}

	public String getPuk(){
		return puk;
	}

	public LocalDate getLastRecharge(){
		return lastRecharge;
	}

	public double getCredit(){
		return credit;
	}

	public Promotion getPromotions(){
		return promotion;
	}

	public Owner getOwner(){
		return owner;
	}

	public boolean isPorted(){
		return ported;
	}

	@Override
	public boolean equals(Object obj){
		if(this==obj)
			return true;
		if(obj==null)
			return false;
		if(getClass()!=obj.getClass())
			return false;
		Sim other=(Sim)obj;
		if(number==null){
			if(other.number!=null)
				return false;
		}
		else
			if(!number.equals(other.number))
				return false;
		return true;
	}

	@Override
	public String toString(){
		return "\tSim [\n\t\tnumber="+number+"\n\t\tpuk="+puk+"\n\t\tlastRecharge="+lastRecharge+"\n\t\tcredit="+credit+"\n\t\tpromotion="+promotion+"\n\t\towner="+owner+"\n\t\tcalls="+calls+"\n\t\tported="+ported+"\n\t]\n";
	}
}
