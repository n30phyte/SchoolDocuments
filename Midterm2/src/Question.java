import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

class Even<T> implements Iterable<T>{
// Add attributes based on the requirements
    T[] outputArray;
    public <C extends Collection<T>> Even(C collection) {
        this.outputArray = (T[]) collection.toArray();
    }

    @Override
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            private int index = 1;
            @Override
            public boolean hasNext() {
                return index < outputArray.length;
            }

            @Override
            public T next() {
                int oldIndex = index;
                index += 2;
                return outputArray[oldIndex];
            }
        };
    }
}

public class Question {

    public static void main(String[] args) {
        List<Character> list = new ArrayList<>();
        String cad = "0H1e3l4l5o6 7W8o9r0l1d2!";
        for (Character chr : cad.toCharArray()) {
            list.add(chr);
        }

        System.out.println(list);

        for (char v : new Even<>(list.subList(0, 12))) {
            System.out.print(v);
        }
        System.out.println();
        for (char v : new Even<>(list.subList(12, list.size()))) {
            System.out.print(v);
        }
    }
}