/**
 * Lab 2: Debugging with an IDE and Prefix Tree)
 */


class Trie {

    static final int ALPHABET_SIZE = 26;

    static class TrieNode {

        TrieNode[] child = new TrieNode[ALPHABET_SIZE];
        boolean isEndOfWord;

        TrieNode() {
            isEndOfWord = false;
            for (int i = 0; i < ALPHABET_SIZE; i++)
                child[i] = null;
        }

    }

    /**
     * Root node of the Prefix Tree
     */
    static TrieNode root;

    public static void insert(String word) {
        // Set current node to prepare for tree descent
        var currentNode = root;

        // Go through each character
        for (var character : word.toCharArray()) {
            // Convert character to an index
            var nodeIndex = (int) Character.toUpperCase(character) - (int) 'A';

            // This means node for letter does not currently exist
            if (currentNode.child[nodeIndex] == null) {
                currentNode.child[nodeIndex] = new TrieNode();
            }

            // Replace node before looping
            currentNode = currentNode.child[nodeIndex];
        }

        // Final node accessed will be the end of the word.
        currentNode.isEndOfWord = true;
    }

    public static boolean search(String word) {
        // Set current node to prepare for tree descent
        var currentNode = root;

        // Go through each character
        for (var character : word.toCharArray()) {
            // Convert character to an index
            var nodeIndex = (int) Character.toUpperCase(character) - (int) 'A';

            // This means node for letter does not exist, definitely false.
            if (currentNode.child[nodeIndex] == null) {
                return false;
            }

            currentNode = currentNode.child[nodeIndex];

        }

        return currentNode.isEndOfWord;
    }

    public static boolean startWith(String prefix) {
        // Set current node to prepare for tree descent
        var currentNode = root;

        // Go through each character
        for (var character : prefix.toCharArray()) {
            // Convert character to an index
            var nodeIndex = (int) Character.toUpperCase(character) - (int) 'A';

            if (currentNode.child[nodeIndex] == null) {
                return false;
            }
            currentNode = currentNode.child[nodeIndex];
        }
        return true;
    }

    public static void main(String[] args) {

        String[] words = {"ece", "lab", "java", "jar", "car",
                "cat", "care", "laboratory", "ebook"};

        String[] output = {"is NOT in the prefix tree", "is in the prefix tree"};

        root = new TrieNode();

        // Construct trie
        int i;
        for (i = 0; i < words.length; i++)
            insert(words[i]);
        

        // Search for different keys
        if (search("lab"))
            System.out.println("lab --- " + output[1]);
        else System.out.println("lab --- " + output[0]);

        if (search("java"))
            System.out.println("java --- " + output[1]);
        else System.out.println("java --- " + output[0]);

        if (startWith("eced"))
            System.out.println("eced --- " + output[1]);
        else System.out.println("eced --- " + output[0]);

        if (startWith("ca"))
            System.out.println("ca --- " + output[1]);
        else System.out.println("ca --- " + output[0]);

        if (search("book"))
            System.out.println("book --- " + output[1]);
        else System.out.println("book --- " + output[0]);

    }


}


