import java.time.Duration;

public class Call {
  public final Duration durata;
  public final String destinatario;
  
  public Call(final Duration durata, final String destinatario) {
    this.durata = durata;
    this.destinatario = destinatario;
  }
  
  Duration getDuration(){
    return durata;
  }
}