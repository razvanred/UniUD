package ro.razvan.uniud.generics;

public class SList<T> {

    private final boolean empty;
    private final T first;
    private final SList<T> rest;

    public SList() {
        first = null;
        rest = null;
        empty = true;
    }

    @SuppressWarnings("unchecked")
    public SList(final T first) {
        this(first, new SList());
    }

    public SList(final T first, final SList<T> rest) {
        this.first = first;
        this.rest = rest;
        empty = false;
    }

    public final T car() {
        return first;
    }

    public final SList<T> cdr() {
        return rest;
    }

    public final boolean isNull() {
        return empty;
    }

    @SuppressWarnings("unchecked")
    public final SList<T> cons(final T element) {
        return new SList(element, this);
    }

    public final SList<T> append(final SList<T> list) {
        if (empty) {
            return list;
        } else {
            return rest.append(list).cons(first);
        }
    }

    public final int length() {
        if (empty) {
            return 0;
        } else {
            return 1 + cdr().length();
        }
    }

    public final T listRef(final int k) {
        if (k == 0) {
            return car();
        } else {
            return cdr().listRef(k - 1);
        }
    }

    public final SList<T> reverse() {
        return reverseRec(new SList());
    }

    @Override
    public String toString() {

        SList cdr = cdr();

        if (empty) {
            return "()";
        } else if (cdr == null || cdr.isNull()) {
            return "(" + first + ")";
        } else {
            final StringBuilder builder = new StringBuilder("(" + first);

            while (cdr != null && !cdr.isNull()) {
                builder.append(", ").append(cdr.first);
                cdr = cdr.cdr();
            }

            return builder.append(")").toString();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if(!(obj instanceof SList)){
            return false;
        }

        final SList list = (SList) obj;

        if(list.isNull() || empty){
            return list.isNull() && empty;
        } else if (list.car() != car()){
            return false;
        } else {
            return list.cdr().equals(cdr());
        }
    }

    private SList<T> reverseRec(final SList<T> acc) {
        if (empty) {
            return acc;
        } else {
            return cdr().reverseRec(acc.cons(first));
        }
    }

}
