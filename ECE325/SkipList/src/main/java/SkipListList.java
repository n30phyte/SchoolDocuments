import java.util.*;

/**
 * SkipList Adaptor to provide the List<T> interface
 * @param <T> {@code T} value type
 */
public class SkipListList<T extends Comparable<T>> extends SkipList<Integer,T> implements List<T>
{

    // *******************************************************
    // Methods required for benchmarking

    @Override
    public boolean isEmpty() {
        return this.size() == 0;
    }

    @Override
    public T get(int i) {
        return this.search(i);
    }

    @Override
    public void add(int i, T t) {
        this.insert(i,t);
    }

    @Override
    public T remove(int i) {
        return super.remove(i);
    }


    // *******************************************************
    // The following methods are not required for benchmarking

    @Override
    public boolean contains(Object o) {
        return false;
    }

    @Override
    public Iterator<T> iterator() {
        return null;
    }

    @Override
    public Object[] toArray() {
        return new Object[0];
    }

    @Override
    public <T1> T1[] toArray(T1[] t1s) {
        return null;
    }

    @Override
    public boolean add(T t) {
        return false;
    }

    @Override
    public boolean remove(Object o) {
        return false;
    }

    @Override
    public boolean containsAll(Collection<?> collection) {
        return false;
    }

    @Override
    public boolean addAll(Collection<? extends T> collection) {
        return false;
    }

    @Override
    public boolean addAll(int i, Collection<? extends T> collection) {
        return false;
    }

    @Override
    public boolean removeAll(Collection<?> collection) {
        return false;
    }

    @Override
    public boolean retainAll(Collection<?> collection) {
        return false;
    }

    @Override
    public void clear() {

    }

    @Override
    public T set(int i, T t) {
        return null;
    }

    @Override
    public int indexOf(Object o) {
        return 0;
    }

    @Override
    public int lastIndexOf(Object o) {
        return 0;
    }

    @Override
    public ListIterator<T> listIterator() {
        return null;
    }

    @Override
    public ListIterator<T> listIterator(int i) {
        return null;
    }

    @Override
    public List<T> subList(int i, int i1) {
        return null;
    }
}


