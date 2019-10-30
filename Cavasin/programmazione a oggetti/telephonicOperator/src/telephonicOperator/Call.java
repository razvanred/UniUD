package telephonicOperator;

import java.time.Duration;
import java.time.LocalDateTime;

public class Call{
	private LocalDateTime start, end;
	private Sim recipient;

	public Call(Sim recipient, LocalDateTime start, LocalDateTime end){
		this.start=start;
		this.end=end;
		this.recipient=recipient;
	}

	public Sim receiver(){
		return recipient;
	}

	public Duration duration(){
		return Duration.between(start,end);
	}

	@Override
	public String toString(){
		return "Call [start="+start+", end="+end+", recipient="+recipient+"]";
	}
}
