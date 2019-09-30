package ro.razvan.uniud.huffman.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import ro.razvan.uniud.huffman.java.utils.ArrayUtils;

final class NodeStack {

    @NotNull
    private Node[] stack = new Node[0];

    /**
     * Verifica se lo stack è vuoto
     *
     * @return true se lo stack è vuoto
     */
    boolean isEmpty() {
        return stack.length == 0;
    }

    /**
     * Restuisce il primo valore dello stack
     *
     * @return primo valore dello stack
     */
    @Nullable
    Node peek() {
        return ArrayUtils.getFirstOrNull(stack);
    }

    /**
     * Restituisce il primo valore nello stack e lo rimuove
     *
     * @return primo valore dello stack
     */
    @Nullable
    Node pop() {

        final var element = peek();

        if (element == null) {
            return null;
        }

        final var stack = new Node[this.stack.length - 1];

        System.arraycopy(this.stack, 1, stack, 0, stack.length);

        this.stack = stack;

        return element;
    }

    /**
     * Aggiunge un nuovo elemento alla cima dello stack
     *
     * @param n nuovo elemento
     */
    void push(@NotNull final Node n){

        final var stack = new Node[this.stack.length + 1];

        stack[0] = n;

        System.arraycopy(this.stack, 0, stack, 1, this.stack.length);

        this.stack = stack;

    }

}
