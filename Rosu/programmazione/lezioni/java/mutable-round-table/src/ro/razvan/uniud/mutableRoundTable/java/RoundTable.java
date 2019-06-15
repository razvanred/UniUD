package ro.razvan.uniud.mutableRoundTable.java;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.intSList.IntSList;

class RoundTable {

    @NotNull
    private IntSList knightsLeft;

    @NotNull
    private IntSList knightsStanding;

    private boolean served;

    RoundTable(final int n) {
        knightsLeft = rangeIter(1, n);
        knightsStanding = IntSList.Companion.getNULL_INT_S_LIST();
    }

    private static IntSList rangeIter(final int start, int end) {
        var list = IntSList.Companion.getNULL_INT_S_LIST();

        while (end >= start) {
            list = list.cons(end);
            end--;
        }

        return list;
    }

    private static IntSList rangeRec(final int start, final int end) {
        if (start > end) {
            return IntSList.Companion.getNULL_INT_S_LIST();
        } else {
            return new IntSList(start, IntSList.Companion.getNULL_INT_S_LIST()).append(rangeRec(start + 1, end));
        }
    }

    static int count(int n) {

        final var roundTable = new RoundTable(n);

        while (n-- > 1) {
            roundTable.serveKnightAndPassPitcher();
        }

        return roundTable.getKnightWithPitcher();
    }

    int getKnightWithPitcher() {
        return knightsLeft.getCar();
    }

    void serveKnight() {
        knightsStanding = knightsStanding.cons(knightsLeft.getCdr().getCar());
        knightsLeft = knightsLeft.getCdr().getCdr().cons(knightsLeft.getCar());
        served = true;
    }

    void handPitcher() {
        knightsLeft = knightsLeft.getCdr().append(new IntSList(knightsLeft.getCar(), IntSList.Companion.getNULL_INT_S_LIST()));
        served = false;
    }

    void serveKnightAndPassPitcher() {
        knightsStanding = knightsStanding.cons(knightsLeft.getCdr().getCar());
        knightsLeft = knightsLeft.getCdr().getCdr().append(new IntSList(knightsLeft.getCar(), IntSList.Companion.getNULL_INT_S_LIST()));
        served = false;
    }

}
