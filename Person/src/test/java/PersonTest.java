import org.junit.jupiter.api.Test;

import java.util.Set;
import java.util.TreeSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PersonTest {

    @Test
    public void TestSort() {
        Set<Person> persons = new TreeSet<>(new PersonComparator());
        Set<Integer> ages = new TreeSet<>();
        for (int i = 0; i < 20; i++) {
            int random = (int) (Math.random() * 80);
            ages.add(random);
            persons.add(new Person(random));
        }

        assertEquals(ages.size(), persons.size());
        for (var person : persons) {
            assertTrue(ages.contains(person.getAge()));
        }
    }
}