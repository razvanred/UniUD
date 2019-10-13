public class NodeQueue{
  Node v[];
  
  public NodeQueue(){         // costruttore: creazione della coda vuota
    v=new Node[0];
  }
  
  public int size(){          // restituisce il numero di elementi contenuti nella coda
    return v.length;
  }
  
  public Node poll(){         // restituisce e rimuove dalla coda l’elemento con “peso minore”
    if(v.length==0){
      return null;
    }else{
      Node t[]=new Node[v.length-1],car=v[0];
      for(int i=0;i<t.length;i++){
        t[i]=v[i+1];
      }
      v=t;
      return car;
    }
  }
  
  public void add(Node n){    // aggiunge un nuovo elemento n alla coda
    int i,p;
    Node t[]=new Node[v.length+1];
    for(p=0;p<v.length&&n.compareTo(v[p])>0;p++);
    for(i=0;i<p;i++){
      t[i]=v[i];
    }
    t[i]=n;
    for(i++;i<t.length;i++){
      t[i]=v[i-1];
    }
    v=t;
  }
  
  public String toString(){
    String s="{ ";
    for(int i=0;i<v.length;i++){
      s+=v[i].weight()+" ";
    }
    return s+"}";
  }
}
