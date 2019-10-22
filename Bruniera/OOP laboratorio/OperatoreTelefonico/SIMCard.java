import java.time.Duration;
import java.util.Date;
import java.util.List;
import java.util.ArrayList;

public class SIMCard {
  public final String number;
  public final String puk;
  private double credito;
  private final List<Call> chiamate;
  private Date ultimaRicarica;
  private Person owner;
  private Prom prom;
  
  public SIMCard(String number, String puk, Person owner) {
    prom = null;
    chiamate = new ArrayList<Call>();
    ultimaRicarica = null;
    this.owner = owner;
    this.number = number;
    this.puk = puk;
    credito = 0;
  }
}