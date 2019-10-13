
public class NodeStack {
	
	private Node[] stack;
	public int index;
	//public static final Node voidNode= new Node('ò',149);
	
	public NodeStack() {
		stack = new Node[128];
		index = 0;
	}
	
	public boolean empty() {
		if(index <= 0) {
			return true;
		}
		return false;
	}
	
	public Node peek() {
		if(!this.empty()) {
			return stack[index - 1];
		}
		index = 0;
		return stack[0];
	}
	
	public Node pop() {
		Node pop;
		if(!this.empty()) {
			pop = stack[index - 1];
			//elimino stack[index] dal vettore stack
			stack[index - 1] = null;
			index = index - 1;
		}else {
			pop = stack[0];
			index = 0;
		}
		return pop;
	}
	
	public void push( Node n ) {
		stack[index] = n;
		index = index + 1;
	}
	

}

/*
 * public NodeStack() costruttore: creazione di uno stack vuoto
public boolean empty() verifica se lo stack è vuoto
public Node peek() restituisce l’elemento in cima allo stack (senza rimuoverlo dallo stack)
public Node pop() restituisce l’elemento in cima allo stack e lo rimuove dallo stack
public void push( Node n ) aggiunge un nuovo elemento n in cima allo stack
 * 
 * 
 * 
 * 
 * */
