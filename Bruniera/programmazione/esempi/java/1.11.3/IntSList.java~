public class IntSList{
    public static final IntSList NULL_INTLIST = new IntSList();
    private final boolean empty;
    private final int first;
    private final IntSList rest;

    public IntSList(){
        empty=true;
        first=0;
        rest=null;
    }

    public IntSList(int first, IntSList rest){
        empty=false;
        this.first=first;
        this.rest=rest;
    }

    public IntSList cdr(){
        return rest;
    }

    public int car(){
        return first;
    }

    public IntSList cons(int n){
        return new IntSList(n, this);
    }

    public boolean nullList(){
        return empty;
    }

    @Override
    public String toString(){
        if(empty){
            return "";
        }
        else if(rest.nullList()){
            return "("+first+")";
        }
        else{
            String v="("+first;
            IntSList q=rest;
            do{
                v+=", "+q.car();
                q=q.cdr();
            }while(!q.nullList());
            return v+")";
        }
    }

    public int length(){
        if(nullList()){
            return 0;
        } else {
            return 1+rest.length();
        }
    }

    public int listRef(int i){
        if(i==0){
            return car();
        } else {
            return rest.listRef(i-1);
        }
    }

    public IntSList append(IntSList r){
        if(nullList()){
            return r;
        } else {
            return rest.append(r).cons(car());
        }
    }

    public IntSList reverse(){
        return reverseRec(NULL_INTLIST);
    }

    private IntSList reverseRec(IntSList r){
        if(nullList()){
            return r;
        } else {
            return rest.reverseRec(r.cons(car()));
        }
    }
}