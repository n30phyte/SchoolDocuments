package ece325;

/**
 * Lab 4: Generics <br /> The {@code GenericTrie} class <br /> Reference: <a
 * href="https://en.wikipedia.org/wiki/Trie"> https://en.wikipedia.org/wiki/Trie
 * </a>
 */
public class GenericTrie<K extends CharSequence, V> {

    /**
     * Root node of the Prefix Tree
     */
    TrieNode<V> root;

    public GenericTrie() {
        root = new TrieNode<>();
    }

    /**
     * Insert a key with value into the Trie
     *
     * @param word  Word to be inserted
     * @param value Value of word
     */
    public void insert(K word, V value) {
        // Set current node to prepare for tree descent
        var currentNode = root;

        // Go through each character
        for (var idx = 0; idx < word.length(); idx++) {

            var nodeIndex = charIndex(word.charAt(idx));

            // This means node for letter does not currently exist
            if (currentNode.child[nodeIndex] == null) {
                currentNode.child[nodeIndex] = new TrieNode<>();
            }

            // Replace node before looping
            currentNode = currentNode.child[nodeIndex];
        }

        // Final node accessed will be the end of the word.
        currentNode.isEndOfWord = true;
        currentNode.value = value;
    }


    /**
     * Search if a word exists and return the key if it does
     *
     * @param word Word to look for
     * @return key stored in the word
     */
    public V search(K word) {
        // Set current node to prepare for tree descent
        var currentNode = root;

        // Go through each character
        for (var idx = 0; idx < word.length(); idx++) {

            // Convert character to an index
            var nodeIndex = charIndex(word.charAt(idx));

            // This means node for letter does not exist, definitely false.
            if (currentNode.child[nodeIndex] == null) {
                return null;
            }

            currentNode = currentNode.child[nodeIndex];

        }

        return currentNode.value;
    }


    /**
     * Check if this prefix exists in the Trie
     *
     * @param prefix prefix to look for
     * @return True if exists, false otherwise.
     */
    public boolean startWith(K prefix) {
        // Set current node to prepare for tree descent
        var currentNode = root;

        // Go through each character
        for (var idx = 0; idx < prefix.length(); idx++) {

            var nodeIndex = charIndex(prefix.charAt(idx));

            if (currentNode.child[nodeIndex] == null) {
                return false;
            }
            currentNode = currentNode.child[nodeIndex];
        }
        return true;
    }

    /**
     * Remove a word and unnecessary parents from the Trie if it exists
     *
     * @param word Word to remove
     * @return Value of key or null if it doesn't exist
     */
    public V remove(K word) {
        TrieNode<V> currentNode = root;
        TrieNode<V> prevNode = null;

        int depth = 0;

        TrieNode<V> tailStart = null;
        int tailLength = 0;

        while (depth < word.length() && currentNode.child[charIndex(word.charAt(depth))] != null) {
            boolean hasOtherChild = false;

            if (currentNode.isEndOfWord) {
                hasOtherChild = true;
            } else {
                for (var i = 0; i < currentNode.child.length; i++) {
                    if (i != charIndex(word.charAt(depth)) && currentNode.child[i] != null) {
                        hasOtherChild = true;
                        break;
                    }
                }
            }

            if (hasOtherChild) {
                // Current node can't be start of tail
                tailStart = null;
                tailLength = 0;
            } else {
                if (tailStart == null) {
                    // No tail start yet, this can be it.
                    tailStart = prevNode;
                }
                tailLength++;
            }

            prevNode = currentNode;
            currentNode = currentNode.child[charIndex(word.charAt(depth++))];
        }

        if (word.length() == depth) {
            var value = currentNode.value;
            if (value != null && tailStart != null) {
                int targetIndex = depth - tailLength - 1;
                tailStart.child[charIndex(word.charAt(targetIndex))] = null;
            }

            return value;
        }

        return null;
    }

    /**
     * Convert a character to an index, with A being 0 and Z being 25
     *
     * @param character Character to get index of
     * @return array index value
     */
    private int charIndex(char character) {
        return (int) Character.toUpperCase(character) - (int) 'A';
    }

}


