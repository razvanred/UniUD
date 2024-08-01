public class Triple<T, U, V> {

    private final T first;
    private final U second;
    private final V third;

    public Triple(T first, U second, V third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }

    public T fst() { return first; }
    public U snd() { return second; }
    public V trd() { return third; }
}
