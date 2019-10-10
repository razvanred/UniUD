public class NodeQueue{
  
  private final Node val;
  private NodeQueue next;
  
  public NodeQueue(){
    val=new Node('\0',0);
    next=null;
  }
  
  private NodeQueue(Node val, NodeQueue next){
    this.val=val;
    this.next=next;
  }
  
  public int size(){
    NodeQueue cur=next;
    int size=0;
    while(cur!=null){
      size++;
      cur=cur.next;
    }
    return size;
  }
  
  public Node poll(){
    if(next==null){
      return null;
    }
    Node v=next.val;
    next=next.next;
    return v;
  }
  
  public void add(Node n){
    NodeQueue cur=this;
    while(cur.next!=null && cur.next.val.compareTo(n)<0 ){
      cur=cur.next;
    }
    cur.next=new NodeQueue(n,cur.next);
  }
  
}