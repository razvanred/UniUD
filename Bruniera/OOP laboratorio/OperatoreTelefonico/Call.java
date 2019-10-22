import java.time.Duration;

public class Call {
  public final Duration durata;
  public final String destinatario;
  
  public Call(Duration durata, String destinatario) {
    this.durata = durata;
    this.destinatario = destinatario;
  }
  
  Duration getDuration(){
    return durata;
  }
}