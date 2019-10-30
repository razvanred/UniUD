public enum Prom {
  MINUTI_ILLIMITATI (15),
  CHIAMA_RICHIAMA (10),
  DATI_UNLIMITED (0);
  
  public final float cost;
  
  Prom(final float cost) {
    this.cost = cost;
  }
}