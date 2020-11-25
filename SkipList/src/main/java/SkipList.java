import java.util.ArrayList;
import java.util.Stack;

/**
 * Lab 5: Java Collection Framework and Skip List <br /> The {@code SkipList} class
 *
 * @param <K> {@code K} key of each skip list node
 * @param <V> {@code V} value of each skip list node
 */
public class SkipList<K extends Comparable<K>, V> {

    /**
     * The {@code Node} class for {@code SkipList}
     */
    private class Node {

        public K key;
        public V value;
        public ArrayList<Node> forwards = new ArrayList<>();

        public Node(K key, V value, int level) {
            this.key = key;
            this.value = value;
            for (int i = 0; i < level; i++) {
                forwards.add(null);
            }
        }

        public int getLevel() {
            return forwards.size();
        }

        /**
         * Gets the next node from the current level.
         * @param level level of next node to query
         * @return Node if available, null otherwise.
         */
        public Node getNext(int level) {
            if (level > getLevel()) {
                return null;
            } else {
                return forwards.get(level - 1);
            }
        }

        /**
         * Inserts a node in a new level
         *
         * @param newNode node to insert
         */
        public void addNext(Node newNode) {
            forwards.add(newNode);
        }


        /**
         * Set a node at specific levels
         *
         * @param level Level to insert node into
         * @param newNode node to insert
         */
        public void setNext(int level, Node newNode) {
            if (level <= getLevel()) {
                forwards.set(level - 1, newNode);
            }
        }

        public String toString() {
            return String.format("%s(%s,%d)", value, key, forwards.size());
        }
    }

    Node head;

    /**
     * Level of the skip list. 1 is lowest level.
     */
    private int level = 1;

    /**
     * Size of the skip list
     */
    private int size = 0;

    public SkipList() {
        head = new Node(null, null, 1);
    }

    /**
     * Insert an new element into the skip list
     *
     * @param key   {@code K} key of the new element
     * @param value {@code V} value of the new element
     */
    public void insert(K key, V value) {
        // Get search stack
        var updateStack = find(key);
        // New node to insert
        var newNode = new Node(key, value, 0);

        // Check out node to insert after at level 1
        var previousNode = updateStack.pop();

        // Check for same key
        if (previousNode.key != null && key.compareTo(previousNode.key) == 0) {
            previousNode.value = value;
        } else {
            var currentLevel = 1;

            newNode.addNext(previousNode.getNext(currentLevel));
            previousNode.setNext(currentLevel, newNode);

            // Flip a coin to decide whether or not we need to add levels
            while (Math.random() < 0.5) {
                currentLevel++;

                if (updateStack.empty()) {
                    newNode.addNext(null);
                } else {
                    previousNode = updateStack.pop();

                    newNode.addNext(previousNode.getNext(currentLevel));
                    previousNode.setNext(currentLevel, newNode);
                }
            }

            while (head.getLevel() < newNode.getLevel()) {
                head.addNext(newNode);
            }

            size++;
            level = head.getLevel();
        }
    }

    /**
     * Generate a list of the last nodes in each level visited to reach the key.
     *
     * @param key Key that's being searched for
     * @return List of nodes on each level before the searched for node. Top of stack is lowest
     * level, bottom of stack is highest level.
     */
    private Stack<Node> find(K key) {
        var output = new Stack<Node>();

        var currentNode = head;
        var currentLevel = this.level();

        while (currentLevel > 0) {
            while (currentNode.getNext(currentLevel) != null
                && currentNode.getNext(currentLevel).key.compareTo(key) < 0) {
                currentNode = currentNode.getNext(currentLevel);
            }
            output.push(currentNode);
            currentLevel--;
        }

        return output;
    }

    /**
     * Remove an element by the key
     *
     * @param key {@code K} key of the element
     * @return {@code V} value of the removed element
     */
    public V remove(K key) {
        var removeStack = find(key);

        if (removeStack.peek().getNext(1).key != null &&
            removeStack.peek().getNext(1).key.compareTo(key) == 0) {
            // Found
            var previousNode = removeStack.peek();
            var removedNode = previousNode.getNext(1);

            int currentLevel = 1;

            while (currentLevel <= removedNode.getLevel() && !removeStack.empty()) {
                previousNode = removeStack.pop();
                previousNode.setNext(currentLevel, removedNode.getNext(currentLevel));
                currentLevel++;
            }

            size--;
            return removedNode.value;
        }
//         Not found
        return null;
    }

    /**
     * Search for an element by the key
     *
     * @param key {@code K} key of the element
     * @return {@code V} value of the target element
     */
    public V search(K key) {
        var searchStack = find(key);

        if (searchStack.peek().getNext(1) == null) {
            return null;
        }

        if (searchStack.peek().getNext(1).key != null
            && searchStack.peek().getNext(1).key.compareTo(key) == 0) {
            return searchStack.peek().getNext(1).value;
        }

        return null;
    }

    /**
     * Get the level of the skip list
     *
     * @return {@code int} level of the skip list
     */
    public int level() {
        return level;
    }

    /**
     * Get the size of the skip list
     *
     * @return {@code int} size of the skip list
     */
    public int size() {
        return size;
    }

    /**
     * Print the skip list
     *
     * @return {@code String} the string format of the skip list
     */
    public String toString() {
        var outputString = new StringBuilder();

        var currentNode = head.getNext(1);
        for (var i = 0; i < size(); i++) {
            outputString.append(currentNode).append("->");
            currentNode = currentNode.getNext(1);
        }
        outputString.append("null");

        return outputString.toString();
    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        SkipList<Integer, String> list = new SkipList<Integer, String>();
        int[] keys = new int[10];
        for (int i = 0; i < 10; i++) {                          // Insert elements
            keys[i] = (int) (Math.random() * 200);
            list.insert(keys[i], "\"" + keys[i] + "\"");
        }

        System.out.println(list);

        for (int i = 0; i < 10; i += 3) {
            int key = keys[i];
            // Search elements
            System.out.printf("Find element             %3d: value=%s%n", key, list.search(key));
//             Remove some elements
            System.out.printf("Remove element           %3d: value=%s%n", key, list.remove(key));
//             Search the removed elements
            System.out.printf("Find the removed element %3d: value=%s%n", key, list.search(key));
        }

        System.out.println(list);
    }

}
