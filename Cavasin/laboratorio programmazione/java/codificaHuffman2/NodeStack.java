public class NodeStack {
  Node v[];
  
  public NodeStack(){              // costruttore: creazione di uno stack vuoto
    v=new Node[0];
  }
  
  public boolean empty(){          // verifica se lo stack è vuoto
    return v.length==0;
  }
  
  public Node peek(){              // restituisce l’elemento in cima allo stack (senza rimuoverlo dallo stack)
    if(v.length==0){
      return null;
    }else{
      return v[0];
    }
  }
  
  public Node pop(){               // restituisce l’elemento in cima allo stack e lo rimuove dallo stack
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
  
  public void push( Node n ){      // aggiunge un nuovo elemento n in cima allo stack
    Node t[]=new Node[v.length+1];
    t[0]=n;
    for(int i=0;i<v.length;i++){
      t[i+1]=v[i];
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
