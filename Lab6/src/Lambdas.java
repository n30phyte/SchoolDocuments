import java.util.Arrays;

interface TwoStringPredicate {
    boolean compare(String a, String b);
}

class StringUtils {

    /**
     * Static helper function for Deliverable 3 - 1
     */
    public static int eFirst(String left, String right) {
        if (left.charAt(0) == 'e') {
            return -1;
        } else if (right.charAt(0) == 'e') {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * String comparison function for Deliverable 3 - 2
     */
    public static String betterString(String left, String right, TwoStringPredicate comparator) {
        return comparator.compare(left, right) ? left : right;
    }
}

public class Lambdas {


    public static void main(String[] args) {
        var words = new String[]{"cower", "survival", "calculation", "matter",
                "weapon", "suffering", "wine", "extraterrestrial", "atmosphere", "fiction"};


        /**
         * Deliverable 3 - 1
         */
        System.out.println("Sorted by length: ");
        Arrays.sort(words, (a, b) -> a.length() - b.length());
        for (var word : words) {
            System.out.println(word);
        }

        System.out.println("\nReverse sorted length: ");
        Arrays.sort(words, (a, b) -> b.length() - a.length());
        for (var word : words) {
            System.out.println(word);
        }

        System.out.println("\nSorted by first character: ");
        Arrays.sort(words, (a, b) -> a.charAt(0) - b.charAt(0));
        for (var word : words) {
            System.out.println(word);
        }

        System.out.println("\nSorted by e first character: ");
        Arrays.sort(words, (a, b) -> {
            if (a.charAt(0) == 'e') {
                return -1;
            } else if (b.charAt(0) == 'e') {
                return 1;
            } else {
                return 0;
            }
        });
        for (var word : words) {
            System.out.println(word);
        }

        System.out.println("\nSorted by e first character (Static Method): ");
        Arrays.sort(words, (a, b) -> StringUtils.eFirst(a, b));
        for (var word : words) {
            System.out.println(word);
        }

        /**
         * Deliverable 3 - 2
         */
        String string1 = "dribble";
        String string2 = "supercalifragilisticexpialidocious";

        String longer = StringUtils.betterString(string1, string2, (a, b) -> a.length() > b.length());

        String first = StringUtils.betterString(string1, string2, (a, b) -> true);

        System.out.format("%s is longer%n", longer);
        System.out.format("%s is first%n", first);

    }
}
