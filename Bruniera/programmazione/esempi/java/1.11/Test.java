public class Test{
    public static void main(String[] args) {
        System.out.println(range(4,7).toString());
    }

    public static  IntSList range(int inf, int sup){
        IntSList q=IntSList.NULL_INTLIST;
        while(sup>=inf){
            q=q.cons(sup);
            sup--;
        }
        return q;
    }
}