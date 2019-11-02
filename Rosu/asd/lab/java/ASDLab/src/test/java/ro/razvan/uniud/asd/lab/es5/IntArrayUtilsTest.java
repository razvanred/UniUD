package ro.razvan.uniud.asd.lab.es5;

import org.junit.Test;

public class IntArrayUtilsTest {

    final int[] array = new int[]{ 1,2,3,4,5,6,7 };

    @Test
    public void binarySearchFoundTest() {
        assert IntArrayUtils.binarySearch(array, 1) == 0;
        assert IntArrayUtils.binarySearch(array, 2) == 1;
        assert IntArrayUtils.binarySearch(array, 3) == 2;
        assert IntArrayUtils.binarySearch(array, 4) == 3;
        assert IntArrayUtils.binarySearch(array, 5) == 4;
        assert IntArrayUtils.binarySearch(array, 6) == 5;
        assert IntArrayUtils.binarySearch(array, 7) == 6;
    }

    @Test
    public void binarySearchNotFoundTest() {
        assert IntArrayUtils.binarySearch(array, 0) == -1;
        assert IntArrayUtils.binarySearch(array, -1) == -1;
        assert IntArrayUtils.binarySearch(array, -200) == -1;
        assert IntArrayUtils.binarySearch(array, 8) == -1;
        assert IntArrayUtils.binarySearch(array, 9) == -1;
        assert IntArrayUtils.binarySearch(array, 200) == -1;
    }
}
