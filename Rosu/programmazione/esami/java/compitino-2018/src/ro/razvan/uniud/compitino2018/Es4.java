package ro.razvan.uniud.compitino2018;

import java.util.ArrayList;
import java.util.List;

public final class Es4 {

    static final class ProximityStructure {

        private final List<Double> measures;

        ProximityStructure() {
            measures = new ArrayList<>();
        }

        final int getSize() {
            return measures.size();
        }

        final void add(final double x) {
            measures.add(x);
        }

        final double removeClosestTo(final double x) {

            final var value = measures.get(removeClosestTo(x, 0, 0));

            measures.remove(value);

            return value;
        }

        private int removeClosestTo(final double x, final int i, final int li) {

            if (measures.size() == i) {
                return li;
            }

            final var value = Math.abs(x - measures.get(i));
            final var t = Math.abs(x - measures.get(li));

            println(t + " vs " + value);

            if (value <= t) {
                return removeClosestTo(x, i + 1, i);
            } else {
                return removeClosestTo(x, i + 1, li);
            }
        }

        List<Double> getMeasures() {
            return measures;
        }

    }

    public static void main(final String[] args) {

        final var measures = new ProximityStructure();

        measures.add(56);
        measures.add(68);
        measures.add(45);
        measures.add(1);

        println(measures.getSize());

        println(measures.removeClosestTo(3));
        println(measures.getMeasures());
        println(measures.removeClosestTo(3));
        println(measures.getMeasures());

    }

    static void println(final Object any) {
        System.out.println(any);
    }

}
