package ro.razvan.uniud.huffman.java;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class Node implements Comparable<Node> {

    private final char character;
    private final int weight;

    @Nullable
    private final Node left;
    @Nullable
    private final Node right;

    Node(final char character, final int weight) {
        this.character = character;
        this.weight = weight;
        left = null;
        this.right = null;
    }

    Node(@NotNull final Node left, @Nullable final Node right) {
        this.left = left;
        this.right = right;
        weight = left.weight + (right != null ? right.weight : 0);
        character = (char) 0;
    }

    char getCharacter() {
        return character;
    }

    int getWeight() {
        return weight;
    }

    @Nullable
    Node getLeft() {
        return left;
    }

    @Nullable
    Node getRight() {
        return right;
    }

    /**
     * @return se il nodo dell'albero di Huffman è una foglia
     */
    boolean isLeaf() {
        return left == null;
    }

    @Override
    public int compareTo(@NotNull Node o) {
        return Integer.compare(weight, o.weight);
    }

    @Override
    public String toString() {
        if (isLeaf()) {
            if (character == '@' || character == '\\') {
                return "\\" + character + ":" + weight;
            } else {
                return character + ":" + weight;
            }
        } else {
            return "@:" + weight;
        }
    }

}
