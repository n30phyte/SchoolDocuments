// Simple linked list implementation
class Node<T>
{
    T value;
    Node<?> next;

    public Node(T val, Node<?> ref) {
        value = val;
        next = ref;
    }

    public void setNext(Node<?> ref) {
        next = ref;
    }

    public Node<?> getNext() {
        return next;
    }

    public String toString() {
        return "["+value+"]";
    }

}

public class Question3 {

    // Append the list a and b, setting a as the front of the list
    // followed by b and returns the new head of the list
    public static <T> Node<?> append(Node<? super T> a, Node<? super T> b) {
        a.setNext(b);
        return a;
    }

    public static void main(String[] args) {
        Node<Number> n1 = new Node<>(89, new Node(3.14, null));
        Node<Long> n2 = new Node<>(123L, null);
        Node<String> n3 = new Node<>("Hello", null);

        // append invocation #1
        Node<?> head = append(n2, n1); // This invocation should not generate any compilation error
        while (head != null) {
            System.out.println(head);
            head = head.getNext();
        }

        // append invocation #2
//        head = append(n3, n1); // This invocation should generate a compilation error
        while (head != null) {
            System.out.println(head);
            head = head.getNext();
        }

    }
}
