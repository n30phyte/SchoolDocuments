import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Vector;

public class Benchmark {

    /**
     * Modifies the given list, changing each element to the result of computing base power the
     * element
     *
     * @param list {@code List<Integer>} of values
     * @param base {@code int} value
     */
    private static void powerN(List<Integer> list, int base) {
        for (int i = 0; i < list.size(); ++i) {
            Integer oldVal = list.get(i);
            Integer newVal = (int) Math.pow(base, oldVal);
            list.set(i, newVal);
        }
    }

    /**
     * Computes the position in the given {@code list} where the given {@code elem} is greater than
     * the current elements in the list.
     *
     * @param list {@code List<T>} of values
     * @param elem {@code T} element to look for
     * @return The position in the list where elem is grater than the element in the list.
     */
    private static <T extends Comparable<T>> int lowerBound(List<T> list, T elem) {
      if (list == null) {
        return 0;
      }
        int end = list.size();
        int begin = 0;
        while (begin < end) {
            int pos = (begin + end) / 2;
            T elemPos = list.get(pos);
            if (elemPos == null) {
                return end;
            }
          if (elem.compareTo(elemPos) == 0) {
            return pos;
          }
          if (elem.compareTo(elemPos) > 0) {
            begin = pos + 1;
          } else {
            end = pos;
          }
        }
        return end;
    }

    /**
     * Execute the bechmark using the given empty {@code list} of elements, and executing the
     * benchmark with the given number of elements specified by {@code numElements}, and repeating
     * each test {@code iteration} times
     *
     * @param list       {@code List<Integer>} empty list to use
     * @param numElemens {@code List<Integer>} list specifying the number of elements to use on each
     *                   test
     * @param iterations {@code int} number of times each test will be executed
     */
    private static void benchmark(List<Integer> list, List<Integer> numElemens, int iterations) {
        Random numGenerator = new Random();

        for (int n : numElemens) {

            list.clear();
            ExecTime timer1 = new ExecTime("Adding " + n + " elements", "ns");
            ExecTime timer2 = new ExecTime("Removing " + n + " elements", "ns");

            for (int i = 0; i < iterations; ++i) {
                // Adding values
                timer1.start();
                for (int k = 0; k < n; ++k) {
                    int val = numGenerator.nextInt(n);
                    int pos = lowerBound(list, val);
                    list.add(pos, val);
                }
                timer1.stop();
                timer1.register();

                // Removing elements at random positions
                timer2.start();
                for (int k = 0; k < n; ++k) {
                    int pos = numGenerator.nextInt(n) % list.size();
                    list.remove(pos);
                }
                timer2.stop();
                timer2.register();

            }

            System.out.println(
                list.getClass().getName() + ",\t" + n + ",\t" + timer1.average() + ",\t" + timer2
                    .average());
     f    }
    }

    public static void main(String[] args) {

        List<Integer> list1 = new ArrayList<Integer>();
        List<Integer> list2 = new LinkedList<>();
        List<Integer> list3 = new Vector<>();
        List<Integer> list4 = new SkipListList<>();

        List<Integer> seq = new ArrayList<Integer>();
        for (int i : Range.range(5, 16)) {
            seq.add(i);
        }
        powerN(seq, 2);

        System.out.println("collection,n,add,remove");
        benchmark(list1, seq, 10);
        benchmark(list2, seq, 10);
        benchmark(list3, seq, 10);
        benchmark(list4, seq, 10);
    }
}