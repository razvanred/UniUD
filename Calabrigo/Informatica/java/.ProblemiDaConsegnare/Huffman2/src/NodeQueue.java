//crea una coda di priorità, ovvero mette in coda gli elementi con peso minore
public class NodeQueue {
	
	private Node[] queue;
	int index;
	
	public NodeQueue() {
		queue = new Node[128];
		index = 0;
	}
	
	public int size() {
		return index;
	}
	
	public Node poll() {
		Node minNode = queue[0];
		int minIdx = 0;
		for(int i = 1;i < index;i++) {
			if(minNode.compareTo(queue[i - 1]) > 0) {
				//il più piccolo è queue[i]
				minNode = queue[i - 1];
				minIdx = i - 1;
			}
		}
		
		//elimino minNode da queue e shifto il vettore
		
		for(int i = minIdx; i < this.size(); i++) {
			queue[i] = queue[i+1];
		}
		index = index - 1;
		
		return minNode;
	}
	
	public void add( Node n) {
		queue[index] = n;
		index = index + 1;
	}
	
}

/*
  	public NodeQueue() costruttore: creazione della coda vuota
	public int size() restituisce il numero di elementi contenuti nella coda
	public Node poll() restituisce e rimuove dalla coda l’elemento con “peso minore”
	public void add( Node n ) aggiunge un nuovo elemento n alla coda
 * */
