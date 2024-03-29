import java.util.PriorityQueue;

class ClosestPair{
  
  private static double[] closestPair(double[] list){
    double[] couple;
    double prev;
    PriorityQueue<Double> order=new PriorityQueue<Double>();
    
    for(int i=list.length-1;i>=0;i--){
      order.add(new Double(list[i]));
    }
    
    couple=new double[]{order.poll().doubleValue(),order.poll().doubleValue()};
    prev=couple[1];
    
    while(order.size()>0){
      double cur=order.poll().doubleValue();
      if(cur-prev < couple[1]-couple[0]){
        couple[0]=prev;
        couple[1]=cur;
      }
      prev=cur;
    }
    
    return couple;
  }
  
}