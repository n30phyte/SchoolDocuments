import java.util.Arrays;

/**
 * Lab 1: Java Basics, Merge Sort and Maven <br />
 * The {@code MergeSort} class
 *
 * @author Michael Kwok
 */
public class MergeSort {

    /**
     * Merge part of mergesort
     *
     * @param a {@code int[]} Sorted left array
     * @param b {@code int[]} Sorted right array
     * @return Sorted combined array
     */
    private static int[] merge(int[] a, int[] b) {
        // Copy each array and add a sentinel value at the end
        var left = Arrays.copyOfRange(a, 0, a.length + 1);
        var right = Arrays.copyOfRange(b, 0, b.length + 1);

        var output = new int[a.length + b.length];

        // Insert sentinel (simulated infinity)
        left[left.length - 1] = Integer.MAX_VALUE;
        right[right.length - 1] = Integer.MAX_VALUE;

        // Running index of left and right respectively.
        int i = 0;
        int j = 0;

        // Go through the items in each array until all are stored in the new array
        for (int k = 0; k < output.length; k++) {
            if (left[i] <= right[j]) {
                output[k] = left[i];
                i++;
            } else {
                output[k] = right[j];
                j++;
            }
        }

        return output;
    }

    /**
     * The recursive part of the MergeSort procedure
     *
     * @param numbers {@code int[]} The integer array to be sorted
     */
    public static int[] sort(int[] numbers) {

        // If numbers.length <= 1, sorted by definition.
        if (numbers.length > 1) {
            // Split into left and right array
            var left = Arrays.copyOfRange(numbers, 0, (numbers.length / 2));
            var right = Arrays.copyOfRange(numbers, (numbers.length / 2), numbers.length);

            // Recursively sort each side
            var leftSorted = sort(left);
            var rightSorted = sort(right);

            // Merge sorted lists
            return merge(leftSorted, rightSorted);
        }

        return numbers;
    }

    /**
     * Main entry: test the HeapSort
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        int[] numbers = new int[10];
        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = (int) (Math.random() * 200);
            System.out.print(numbers[i] + " ");
        }
        System.out.println();

        numbers = sort(numbers);

        for (int n : numbers)
            System.out.print(n + " ");
        System.out.println();
    }

}
