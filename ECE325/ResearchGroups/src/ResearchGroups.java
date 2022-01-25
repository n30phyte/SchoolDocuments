import java.util.Arrays;

/**
 * Lab 2: Debugging with an IDE and Prefix Tree) <br />
 * The {@code ResearchGroup} class uses a 2D array to store the names of group members
 */


public class ResearchGroups {

    /**
     * Search a person to check whether he/she is in the groups
     *
     * @param groups {@code String[]} The 2D array of groups to be searched
     * @param name   {@code String} name of the person
     */
    public static void searchMember(String[][] groups, String name) {
        var groupCount = 0;
        boolean groupLeader = false;

        for (var group : groups) {
            for (var j = 0; j < group.length; j++) {

                var person = group[j];

                if (person.equals(name)) {
                    groupCount++;
                    if (j == 0) {
                        groupLeader = true;
                    }
                }
            }
        }

        if (groupCount == 0) {
            // Person was not in any group
            System.out.format("%s was not found in any group.%n", name);
        } else {
            // Person was found
            System.out.format("%s was found in %d group(s). %s %s a leader.%n", name, groupCount, name, groupLeader ? "is" : "is not");
        }

    }

    /**
     * Merge part of mergesort
     *
     * @param groupA {@code String[][]} Sorted left array
     * @param groupB {@code String[][]} Sorted right array
     * @return Sorted combined array
     */
    private static String[][] merge(String[][] groupA, String[][] groupB) {
        var output = new String[groupA.length + groupB.length][];

        // Running index of left and right respectively.
        int i = 0;
        int j = 0;

        // Go through the items in each array until all are stored in the new array
        for (int k = 0; k < output.length; k++) {
            // If left still in bounds and ( right out of bounds or left is less than or equal to right)
            // copy to output
            if (i < groupA.length && (j >= groupB.length || groupA[i].length <= groupB[j].length)) {
                output[k] = groupA[i];
                i++;
            } else {
                output[k] = groupB[j];
                j++;
            }
        }

        return output;
    }

    /**
     * Recursive mergesort algorithm implementation
     * This is the main sort function
     *
     * @param groups {@code String[][]} A jagged array of the research groups
     * @return A sorted jagged array based on the size of each group
     */
    private static String[][] sort(String[][] groups) {
        if (groups.length > 1) {
            // Split into left and right array
            var left = Arrays.copyOfRange(groups, 0, (groups.length / 2));
            var right = Arrays.copyOfRange(groups, (groups.length / 2), groups.length);

            // Recursively sort each side
            var leftSorted = sort(left);
            var rightSorted = sort(right);

            // Merge sorted lists
            return merge(leftSorted, rightSorted);
        }

        return groups;
    }


    /**
     * Sort groups by number of members <br />
     *
     * @param groups {@code String[][]} The 2D array of groups to be sorted
     */
    public static void sortGroups(String[][] groups) {
        var sortedGroups = sort(groups);
        for (var group : sortedGroups) {
            for (var person : group) {
                System.out.print(person + " ");
            }
            System.out.println();
        }

    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        String[][] groups = {{"Bob", "Carol", "Eric", "Matt"},             // 0
                {"Jim", "Lucy", "Terry", "Brenda", "Ben"},    // 1
                {"Susan", "Brad", "Jim"},                     // 2
                {"Sue", "Wendy", "Sam"},                      // 3
                {"Kate", "Jack", "James", "Sydney"},          // 4
                {"Mohammad", "Tim", "Kian"},                  // 5
                {"Emma", "Carol"},                            // 6
                {"Nick", "Osama", "Harry", "Ben"},            // 7
                {"Mary", "John", "Ricky"}};                  // 8

        ResearchGroups.searchMember(groups, "Jim");
        ResearchGroups.searchMember(groups, "Lucy");
        ResearchGroups.searchMember(groups, "John Doe");
        ResearchGroups.sortGroups(groups);
    }

}
