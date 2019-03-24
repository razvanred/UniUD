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
        this.empty=rest;
    }

    public IntSList cdr(){
        return rest;
    }

    public int car(){
        return first;
    }

    public IntSList cons(int n){
        return new IntSList(n, this)
    }

    public boolean isEmpty(){
        return empty;
    }
}