public class NodeStack{
  
  private final Node val;
  private NodeStack next;
  
  public NodeStack(){
    val=null;
    next=null;
  }
  
  private NodeStack(Node val, NodeStack next){
    this.val=val;
    this.next=next;
  }
  
  public boolean empty(){
    return next==null;
  }
   
  public Node peek(){
    return next.val;
  }
  
  public Node pop(){
    if(next==null){
      return null;
    }
    Node n=next.val;
    next=next.next;
    return n;
  }
  
  public void push(Node n){
    next=new NodeStack(n, next);
  }
  
}