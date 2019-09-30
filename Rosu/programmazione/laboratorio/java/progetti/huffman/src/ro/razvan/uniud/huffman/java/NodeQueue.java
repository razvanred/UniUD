package ro.razvan.uniud.huffman.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static java.util.Arrays.deepToString;

public final class NodeQueue {

    @NotNull
    private Node[] queue;

    public NodeQueue() {
        queue = new Node[0];
    }

    /**
     * Aggiunge l'elemento alla queue mantenendo l'array ordinato
     *
     * @param n nuovo nodo da aggiungere alla queue
     */
    public void add(@NotNull final Node n) {

        final var size = size();

        var enough = false;

        final var newQueue = new Node[size + 1];

        for (int i = 0, j = 0; i < size; i++, j++) {

            if (queue[i].compareTo(n) > 0 && !enough) {
                newQueue[j++] = n;
                enough = true;
            }

            newQueue[j] = queue[i];

        }

        if (size == 0 || !enough) {
            newQueue[size] = n;
        }

        queue = newQueue;
    }

    /**
     * Rimuove dall'array l'elemento di minor peso e lo resituisce
     *
     * @return nodo di minor peso presente nell'array
     */
    @Nullable
    public Node poll() {

        final var size = size();

        if (size == 0) {
            return null;
        }

        final var element = queue[0];
        final var newQueue = new Node[size - 1];

        System.arraycopy(queue, 1, newQueue, 0, newQueue.length);

        queue = newQueue;

        return element;
    }

    /**
     * Trova la dimensione della queue
     *
     * @return dimensione della queue
     */
    public int size() {
        return queue.length;
    }

    @Override
    public String toString() {
        return deepToString(queue);
    }

}
