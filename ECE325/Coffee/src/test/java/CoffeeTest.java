import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CoffeeTest {
    private static List<Coffee> coffees;

    @BeforeAll
    static void Setup() {
        coffees = new ArrayList<>();

        coffees.add(new Coffee(10));
        coffees.add(new Coffee(2));
        coffees.add(new Coffee(10));
        coffees.add(new Coffee(20));
        coffees.add(new Coffee(5));

        Collections.sort(coffees);
    }

    @Test
    void firstStrength() {
        assertEquals(2, coffees.get(0).getStrength());
    }

    @Test
    void finalStrength() {
        assertEquals(20, coffees.get(coffees.size()-1).getStrength());
    }

    @Test
    void orderedStrengths() {
        assertEquals(2, coffees.get(0).getStrength());
        assertEquals(5, coffees.get(1).getStrength());
        assertEquals(10, coffees.get(2).getStrength());
        assertEquals(10, coffees.get(3).getStrength());
        assertEquals(20, coffees.get(4).getStrength());
    }

    @Test
    void randomStrength() {
        var strengths = new ArrayList<Integer>();
        var randCoffees = new ArrayList<Coffee>();

        for(var i = 0; i < 10; i++) {
            int random = (int) (Math.random() * 100);
            strengths.add(random);
            randCoffees.add(new Coffee(random));
        }

        Collections.sort(strengths);
        Collections.sort(randCoffees);

        for(var i = 0; i < 10; i++) {
            assertEquals(strengths.get(i), randCoffees.get(i).getStrength());
        }
    }
}