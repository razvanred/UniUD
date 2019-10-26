package telephonicOperator;

import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;

public class Sim{
	private String number, puk;
	private LocalDate lastRecharge;
	private double credit;
	private byte promotions;
	private Owner owner;
	private ArrayList<Call> calls;
	private boolean ported;

	public Sim(String number, String puk, double credit, byte promotions, Owner owner){
		this.number=number;
		this.puk=puk;
		this.credit=credit;
		this.promotions=promotions;
		this.owner=owner;
		lastRecharge=LocalDate.now();
		calls=new ArrayList<Call>();
		ported=false;
	}

	public Sim(Sim sim){
		number=sim.getNumber();
		puk=sim.getPuk();
		lastRecharge=sim.getLastRecharge();
		credit=sim.getCredit();
		promotions=sim.getPromotions();
		owner=sim.getOwner();
		calls=new ArrayList<Call>();
		ported=true;
	}

	public void addCall(Call call){
		calls.add(call);
	}

	public Duration callTime(){
		Duration d=Duration.ZERO;

		for(Call t:calls){
			d.plus(t.duration());
		}
		return d;
	}

	public ArrayList<Call> calls(){
		return calls;
	}

	public ArrayList<Call> callsTo(Sim sim){
		ArrayList<Call> t=new ArrayList<Call>();

		for(Call c:calls){
			if(sim.equals(c.receiver())){
				t.add(c);
			}
		}
		return t;
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

	public byte getPromotions(){
		return promotions;
	}

	public Owner getOwner(){
		return owner;
	}

	public boolean isPorted(){
		return ported;
	}
}
